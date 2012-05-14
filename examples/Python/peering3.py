#
#   Broker peering simulation (part 3) in Python
#   Prototypes the full flow of status and tasks
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
NBR_WORKERS = 5


def client_task(name, i):
    """Request-reply client using REQ socket"""
    ctx = zmq.Context()
    client = ctx.socket(zmq.REQ)
    client.identity = "Client-%s-%s" % (name, i)
    client.connect("ipc://%s-localfe.ipc" % name)
    monitor = ctx.socket(zmq.PUSH)
    monitor.connect("ipc://%s-monitor.ipc" % name)

    poller = zmq.Poller()
    poller.register(client, zmq.POLLIN)
    while True:
        time.sleep(random.randint(0, 5))
        for _ in range(random.randint(0, 15)):
            # send request with random hex ID
            task_id = "%04X" % random.randint(0, 10000)
            client.send(task_id)

            # wait max 10 seconds for a reply, then complain
            try:
                events = dict(poller.poll(10000))
            except zmq.ZMQError:
                return # interrupted

            if events:
                reply = client.recv()
                assert reply == task_id, "expected %s, got %s" % (task_id, reply)
                monitor.send(reply)
            else:
                monitor.send("E: CLIENT EXIT - lost task %s" % task_id)
                return

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
        # Workers are busy for 0/1 seconds
        time.sleep(random.randint(0, 1))
        worker.send_multipart(msg)

def main(myself, peers):
    print "I: preparing broker at %s..." % myself

    # Prepare our context and sockets
    ctx = zmq.Context()

    # Bind cloud frontend to endpoint
    cloudfe = ctx.socket(zmq.ROUTER)
    cloudfe.setsockopt(zmq.IDENTITY, myself)
    cloudfe.bind("ipc://%s-cloud.ipc" % myself)

    # Bind state backend / publisher to endpoint
    statebe = ctx.socket(zmq.PUB)
    statebe.bind("ipc://%s-state.ipc" % myself)

    # Connect cloud and state backends to all peers
    cloudbe = ctx.socket(zmq.ROUTER)
    statefe = ctx.socket(zmq.SUB)
    statefe.setsockopt(zmq.SUBSCRIBE, "")
    cloudbe.setsockopt(zmq.IDENTITY, myself)

    for peer in peers:
        print "I: connecting to cloud frontend at", peer
        cloudbe.connect("ipc://%s-cloud.ipc" % peer)
        print "I: connecting to state backend at", peer
        statefe.connect("ipc://%s-state.ipc" % peer)

    # Prepare local frontend and backend
    localfe = ctx.socket(zmq.ROUTER)
    localfe.bind("ipc://%s-localfe.ipc" % myself)
    localbe = ctx.socket(zmq.ROUTER)
    localbe.bind("ipc://%s-localbe.ipc" % myself)

    # Prepare monitor socket
    monitor = ctx.socket(zmq.PULL)
    monitor.bind("ipc://%s-monitor.ipc" % myself)

    # Get user to tell us when we can start...
    # raw_input("Press Enter when all brokers are started: ")

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
    # Publish-subscribe flow
    # - Poll statefe and process capacity updates
    # - Each time capacity changes, broadcast new value
    # Request-reply flow
    # - Poll primary and process local/cloud replies
    # - While worker available, route localfe to local or cloud

    local_capacity = 0
    cloud_capacity = 0
    workers = []

    # setup backend poller
    pollerbe = zmq.Poller()
    pollerbe.register(localbe, zmq.POLLIN)
    pollerbe.register(cloudbe, zmq.POLLIN)
    pollerbe.register(statefe, zmq.POLLIN)
    pollerbe.register(monitor, zmq.POLLIN)

    while True:
        # If we have no workers anyhow, wait indefinitely
        try:
            events = dict(pollerbe.poll(1000 if local_capacity else None))
        except zmq.ZMQError:
            break  # interrupted

        previous = local_capacity
        # Handle reply from local worker
        msg = None
        if localbe in events:
            msg = localbe.recv_multipart()
            (address, empty), msg = msg[:2], msg[2:]
            workers.append(address)
            local_capacity += 1

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

        # Handle capacity updates
        if statefe in events:
            peer, s = statefe.recv_multipart()
            cloud_capacity = int(s)

        # handle monitor message
        if monitor in events:
            print "%s\n" % monitor.recv(),


        # Now route as many clients requests as we can handle
        # - If we have local capacity we poll both localfe and cloudfe
        # - If we have cloud capacity only, we poll just localfe
        # - Route any request locally if we can, else to cloud
        while local_capacity + cloud_capacity:
            secondary = zmq.Poller()
            secondary.register(localfe, zmq.POLLIN)
            if local_capacity:
                secondary.register(cloudfe, zmq.POLLIN)
            events = dict(secondary.poll(0))

            # We'll do peer brokers first, to prevent starvation
            if cloudfe in events:
                msg = cloudfe.recv_multipart()
            elif localfe in events:
                msg = localfe.recv_multipart()
            else:
                break  # No work, go back to backends

            if local_capacity:
                msg = [workers.pop(0), ''] + msg
                localbe.send_multipart(msg)
                local_capacity -= 1
            else:
                # Route to random broker peer
                msg = [random.choice(peers), ''] + msg
                cloudbe.send_multipart(msg)
        if local_capacity != previous:
            statebe.send_multipart([myself, "%i" % local_capacity])

if __name__ == '__main__':
    if len(sys.argv) >= 2:
        main(myself=sys.argv[1], peers=sys.argv[2:])
    else:
        print "Usage: peering3.py <me> [<peer_1> [... <peer_N>]]"
        sys.exit(1)
