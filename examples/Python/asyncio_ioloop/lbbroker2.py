#!/usr/bin/env python

"""
synopsis:
    Least-recently used (LRU) queue device
    Clients and workers are shown here in-process
    Author: Guillaume Aubert (gaubert) <guillaume(dot)aubert(at)gmail(dot)com>
    Modified for async/ioloop: Dave Kuhlman <dkuhlman(at)davekuhlman(dot)org>
usage:
    python lbbroker2.py
"""

from __future__ import print_function
import sys
import asyncio
import zmq
from zmq.asyncio import Context, Poller, ZMQEventLoop


NBR_CLIENTS = 10
NBR_WORKERS = 3
NBR_REQUESTS = 3


DEBUG = False


def printdbg(*args):
    if DEBUG:
        print(*args)


def run_worker(worker_url, context, idx):
    """ Worker using REQ socket to do LRU routing """
    printdbg('run_worker {} starting'.format(idx))
    socket = context.socket(zmq.REQ)
    # set worker identity
    socket.identity = (u"Worker-%d" % (idx)).encode('ascii')
    socket.connect(worker_url)
    # Tell the broker we are ready for work
    yield from socket.send(b"READY")
    try:
        while True:
            address, empty, request = yield from socket.recv_multipart()
            msg = request.decode('ascii')
            print('{} received request "{}"'.format(
                socket.identity.decode('ascii'),
                msg
            ))
            msg = '{}-OK'.format(msg).encode('ascii')
            yield from socket.send_multipart([address, b'', msg])
            print('{} sent reply "{}"'.format(
                socket.identity.decode('ascii'),
                msg
            ))
    except asyncio.CancelledError:
        printdbg('worker {} cancelled.')
        return


def run_client(client_url, context, idx):
    """ Basic request-reply client using REQ socket """
    printdbg('run_client {} starting'.format(idx))
    socket = context.socket(zmq.REQ)
    # Set client identity. Makes tracing easier
    socket.identity = (u"Client-%d" % (idx)).encode('ascii')
    socket.connect(client_url)
    for count in range(NBR_REQUESTS):
        #  Send request, get reply
        msg = 'Hello-{}'.format(count)
        msg = msg.encode('ascii')
        yield from socket.send(msg)
        print('{} sent msg: "{}"'.format(
            socket.identity.decode('ascii'),
            msg))
        reply = yield from socket.recv()
        print('{} received reply: "{}"'.format(
            socket.identity.decode('ascii'),
            reply.decode('ascii')))
    printdbg('client {} finished'.format(idx))


@asyncio.coroutine
def run_broker(loop):
    """ main broker method """
    print('(run_broker) starting')
    url_worker = "inproc://workers"
    url_client = "inproc://clients"
    client_nbr = NBR_CLIENTS * 3
    # Prepare our context and sockets
    context = Context()
    frontend = context.socket(zmq.ROUTER)
    frontend.bind(url_client)
    backend = context.socket(zmq.ROUTER)
    backend.bind(url_worker)
    print('(run_broker) creating workers and clients')
    # create workers and clients threads
    worker_tasks = []
    for idx in range(NBR_WORKERS):
        task = asyncio.ensure_future(run_worker(url_worker, context, idx))
        worker_tasks.append(task)
    client_tasks = []
    for idx in range(NBR_CLIENTS):
        task = asyncio.ensure_future(run_client(url_client, context, idx))
        client_tasks.append(task)
    print('(run_broker) after creating workers and clients')
    # Logic of LRU loop
    # - Poll backend always, frontend only if 1+ worker ready
    # - If worker replies, queue worker as ready and forward reply
    # to client if necessary
    # - If client requests, pop next worker and send request to it
    # Queue of available workers
    available_workers = 0
    workers_list = []
    # init poller
    poller = Poller()
    # Always poll for worker activity on backend
    poller.register(backend, zmq.POLLIN)
    # Poll front-end only if we have available workers
    poller.register(frontend, zmq.POLLIN)
    while True:
        socks = yield from poller.poll()
        socks = dict(socks)
        # Handle worker activity on backend
        if (backend in socks and socks[backend] == zmq.POLLIN):
            # Queue worker address for LRU routing
            message = yield from backend.recv_multipart()
            assert available_workers < NBR_WORKERS
            worker_addr = message[0]
            # add worker back to the list of workers
            available_workers += 1
            workers_list.append(worker_addr)
            #   Second frame is empty
            empty = message[1]
            assert empty == b""
            # Third frame is READY or else a client reply address
            client_addr = message[2]
            # If client reply, send rest back to frontend
            if client_addr != b'READY':
                # Following frame is empty
                empty = message[3]
                assert empty == b""
                reply = message[4]
                yield from frontend.send_multipart([client_addr, b"", reply])
                printdbg('(run_broker) to frontend -- reply: "{}"'.format(
                    reply))
                client_nbr -= 1
                if client_nbr == 0:
                    printdbg('(run_broker) exiting')
                    break   # Exit after N messages
        # poll on frontend only if workers are available
        if available_workers > 0:
            if (frontend in socks and socks[frontend] == zmq.POLLIN):
                # Now get next client request, route to LRU worker
                # Client request is [address][empty][request]
                response = yield from frontend.recv_multipart()
                [client_addr, empty, request] = response
                assert empty == b""
                #  Dequeue and drop the next worker address
                available_workers += -1
                worker_id = workers_list.pop()
                yield from backend.send_multipart(
                    [worker_id, b"", client_addr, b"", request])
                printdbg('(run_broker) to backend -- request: "{}"'.format(
                    request))
    #out of infinite loop: do some housekeeping
    printdbg('(run_broker) finished')
    for worker_task in worker_tasks:
        worker_task.cancel()
    printdbg('(run_broker) workers cancelled')
    yield from asyncio.sleep(1)
    frontend.close()
    backend.close()
    #context.term()     # Caution: calling term() blocks.
    loop.stop()
    printdbg('(run_broker) returning')
    return 'finished ok'


@asyncio.coroutine
def run(loop):
    reply = yield from run_broker(loop)
    printdbg('(run) reply: "{}"'.format(reply))


def main():
    args = sys.argv[1:]
    if len(args) != 0:
        sys.exit(__doc__)
    try:
        loop = ZMQEventLoop()
        asyncio.set_event_loop(loop)
        loop.run_until_complete(run(loop))
    except KeyboardInterrupt:
        print('\nFinished (interrupted)')


if __name__ == "__main__":
    #import pdb; pdb.set_trace()
    main()
