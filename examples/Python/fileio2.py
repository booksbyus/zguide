# File Transfer model #2
#
# In which the client requests each chunk individually, thus
# eliminating server queue overflows, but at a cost in speed.

import os
from threading import Thread

import zmq

from zhelpers import socket_set_hwm, zpipe

CHUNK_SIZE = 250000
PIPELINE = 10

def client_thread(ctx, pipe):
    dealer = ctx.socket(zmq.DEALER)
    socket_set_hwm(dealer, PIPELINE)
    dealer.connect("tcp://127.0.0.1:6000")

    credit = PIPELINE   # Up to PIPELINE chunks in transit

    total = 0           # Total bytes received
    chunks = 0          # Total chunks received
    offset = 0          # Offset of next chunk request

    while True:
        while credit:
            # ask for next chunk
            dealer.send_multipart([
                b"fetch",
                b"%i" % total,
                b"%i" % CHUNK_SIZE,
            ])

            offset += CHUNK_SIZE
            credit -= 1

        try:
            chunk = dealer.recv()
        except zmq.ZMQError as e:
            if e.errno == zmq.ETERM:
                return   # shutting down, quit
            else:
                raise

        chunks += 1
        credit += 1
        size = len(chunk)
        total += size
        if size < CHUNK_SIZE:
            break   # Last chunk received; exit

    print ("%i chunks received, %i bytes" % (chunks, total))
    pipe.send(b"OK")

# The rest of the code is exactly the same as in model 2, except
# that we set the HWM on the server's ROUTER socket to PIPELINE
# to act as a sanity check.
# .skip

def server_thread(ctx):
    file = open("testdata", "r")

    router = ctx.socket(zmq.ROUTER)
    socket_set_hwm(router, PIPELINE)
    router.bind("tcp://*:6000")

    while True:
        # First frame in each message is the sender identity
        # Second frame is "fetch" command
        try:
            msg = router.recv_multipart()
        except zmq.ZMQError as e:
            if e.errno == zmq.ETERM:
                return   # shutting down, quit
            else:
                raise

        identity, command, offset_str, chunksz_str = msg

        assert command == b"fetch"

        offset = int(offset_str)
        chunksz = int(chunksz_str)

        # Read chunk of data from file
        file.seek(offset, os.SEEK_SET)
        data = file.read(chunksz)

        # Send resulting chunk to client
        router.send_multipart([identity, data])

# The main task is just the same as in the first model.
# .skip

def main():

    # Start child threads
    ctx = zmq.Context()
    a,b = zpipe(ctx)

    client = Thread(target=client_thread, args=(ctx, b))
    server = Thread(target=server_thread, args=(ctx,))
    client.start()
    server.start()

    # loop until client tells us it's done
    try:
        print a.recv()
    except KeyboardInterrupt:
        pass
    del a,b
    ctx.term()

if __name__ == '__main__':
    main()
