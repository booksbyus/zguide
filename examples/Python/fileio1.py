#  File Transfer model #1
#
#  In which the server sends the entire file to the client in
#  large chunks with no attempt at flow control.

from threading import Thread

import zmq

from zhelpers import socket_set_hwm, zpipe

CHUNK_SIZE = 250000

def client_thread(ctx, pipe):
    dealer = ctx.socket(zmq.DEALER)
    dealer.connect("tcp://127.0.0.1:6000")
    dealer.send(b"fetch")

    total = 0       # Total bytes received
    chunks = 0      # Total chunks received

    while True:
        try:
            chunk = dealer.recv()
        except zmq.ZMQError as e:
            if e.errno == zmq.ETERM:
                return   # shutting down, quit
            else:
                raise

        chunks += 1
        size = len(chunk)
        total += size
        if size == 0:
            break   # whole file received

    print ("%i chunks received, %i bytes" % (chunks, total))
    pipe.send(b"OK")

# File server thread
# The server thread reads the file from disk in chunks, and sends
# each chunk to the client as a separate message. We only have one
# test file, so open that once and then serve it out as needed:


def server_thread(ctx):
    file = open("testdata", "r")

    router = ctx.socket(zmq.ROUTER)

    # Default HWM is 1000, which will drop messages here
    # since we send more than 1,000 chunks of test data,
    # so set an infinite HWM as a simple, stupid solution:
    socket_set_hwm(router, 0)
    router.bind("tcp://*:6000")

    while True:
        # First frame in each message is the sender identity
        # Second frame is "fetch" command
        try:
            identity, command = router.recv_multipart()
        except zmq.ZMQError as e:
            if e.errno == zmq.ETERM:
                return   # shutting down, quit
            else:
                raise

        assert command == b"fetch"

        while True:
            data = file.read(CHUNK_SIZE)
            router.send_multipart([identity, data])
            if not data:
                break

# File main thread
# The main task starts the client and server threads; it's easier
# to test this as a single process with threads, than as multiple
# processes:

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
