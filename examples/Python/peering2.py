#
#   Broker peering simulation (part 2) in Python
#   Prototypes the request-reply flow
#
#   While this example runs in a single process, that is just to make
#   it easier to start and stop the example. Each thread has its own
#   context and conceptually acts as a separate process.
#
#   Author : Min RK
#   Contact: benjaminrk(at)gmail(dot)com
#
import random
import sys
import threading
import time

import zmq


NBR_CLIENTS = 10
NBR_WORKERS = 3


def client_task(name, i):
    """Request-reply client using REQ socket"""
    ctx = zmq.Context()
    client = ctx.socket(zmq.REQ)
    client.identity = "Client-%s-%s" % (name, i)
    client.connect("ipc://%s-localfe.ipc" % name)
    while True:
        client.send("HELLO")
        try:
            reply = client.recv()
        except zmq.ZMQError:
            # interrupted
            return
        print "Client-%s: %s\n" % (i, reply),
        time.sleep(1)


def worker_task(name, i):
    """Worker using REQ socket to do LRU routing"""
    ctx = zmq.Context()
    worker = ctx.socket(zmq.REQ)
    worker.identity = "Worker-%s-%s" % (name, i)
    worker.connect("ipc://%s-localbe.ipc" % name)

    # Tell broker we're ready for work
    worker.send("READY")

    # Process messages as they arrive
    while True:
        try:
            msg = worker.recv_multipart()
        except zmq.ZMQError:
            # interrupted
            return
        print "Worker-%s: %s\n" % (i, msg),
        msg[-1] = "OK"
        worker.send_multipart(msg)

def main(myself, peers):
    print "I: preparing broker at %s..." % myself

    # Prepare our context and sockets
    ctx = zmq.Context()

    # Bind cloud frontend to endpoint
    cloudfe = ctx.socket(zmq.ROUTER)
    cloudfe.setsockopt(zmq.IDENTITY, myself)
    cloudfe.bind("ipc://%s-cloud.ipc" % myself)

    # Connect cloud backend to all peers
    cloudbe = ctx.socket(zmq.ROUTER)
    cloudbe.setsockopt(zmq.IDENTITY, myself)
    for peer in peers:
        print "I: connecting to cloud frontend at", peer
        cloudbe.connect("ipc://%s-cloud.ipc" % peer)

    # Prepare local frontend and backend
    localfe = ctx.socket(zmq.ROUTER)
    localfe.bind("ipc://%s-localfe.ipc" % myself)
    localbe = ctx.socket(zmq.ROUTER)
    localbe.bind("ipc://%s-localbe.ipc" % myself)

    # Get user to tell us when we can start...
    raw_input("Press Enter when all brokers are started: ")

    # create workers and clients threads
    for i in range(NBR_WORKERS):
        thread = threading.Thread(target=worker_task, args=(myself, i))
        thread.daemon = True
        thread.start()

    for i in range(NBR_CLIENTS):
        thread_c = threading.Thread(target=client_task, args=(myself, i))
        thread_c.daemon = True
        thread_c.start()

    # Interesting part
    # -------------------------------------------------------------
    # Request-reply flow
    # - Poll backends and process local/cloud replies
    # - While worker available, route localfe to local or cloud

    workers = []

    # setup pollers
    pollerbe = zmq.Poller()
    pollerbe.register(localbe, zmq.POLLIN)
    pollerbe.register(cloudbe, zmq.POLLIN)

    pollerfe = zmq.Poller()
    pollerfe.register(localfe, zmq.POLLIN)
    pollerfe.register(cloudfe, zmq.POLLIN)

    while True:
        # If we have no workers anyhow, wait indefinitely
        try:
            events = dict(pollerbe.poll(1000 if workers else None))
        except zmq.ZMQError:
            break  # interrupted

        # Handle reply from local worker
        msg = None
        if localbe in events:
            msg = localbe.recv_multipart()
            (address, empty), msg = msg[:2], msg[2:]
            workers.append(address)

            # If it's READY, don't route the message any further
            if msg[-1] == 'READY':
                msg = None
        elif cloudbe in events:
            msg = cloudbe.recv_multipart()
            (address, empty), msg = msg[:2], msg[2:]

            # We don't use peer broker address for anything

        if msg is not None:
            address = msg[0]
            if address in peers:
                # Route reply to cloud if it's addressed to a broker
                cloudfe.send_multipart(msg)
            else:
                # Route reply to client if we still need to
                localfe.send_multipart(msg)

        # Now route as many clients requests as we can handle
        while workers:
            events = dict(pollerfe.poll(0))
            reroutable = False
            # We'll do peer brokers first, to prevent starvation
            if cloudfe in events:
                msg = cloudfe.recv_multipart()
                reroutable = False
            elif localfe in events:
                msg = localfe.recv_multipart()
                reroutable = True
            else:
                break  # No work, go back to backends

            # If reroutable, send to cloud 20% of the time
            # Here we'd normally use cloud status information
            if reroutable and peers and random.randint(0, 4) == 0:
                # Route to random broker peer
                msg = [random.choice(peers), ''] + msg
                cloudbe.send_multipart(msg)
            else:
                msg = [workers.pop(0), ''] + msg
                localbe.send_multipart(msg)

if __name__ == '__main__':
    if len(sys.argv) >= 2:
        main(myself=sys.argv[1], peers=sys.argv[2:])
    else:
        print "Usage: peering2.py <me> [<peer_1> [... <peer_N>]]"
        sys.exit(1)
