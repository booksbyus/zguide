"""
Titanic service

Implements server side of http:#rfc.zeromq.org/spec:9

Author: Min RK <benjaminrk@gmail.com>
"""

import cPickle as pickle
import os
import sys
import threading
import time
from uuid import uuid4

import zmq

from mdwrkapi import MajorDomoWorker
from mdcliapi import MajorDomoClient

from zhelpers import zpipe

TITANIC_DIR = ".titanic"

def request_filename (uuid):
    """Returns freshly allocated request filename for given UUID"""
    return os.path.join(TITANIC_DIR, "%s.req" % uuid)

#

def reply_filename (uuid):
    """Returns freshly allocated reply filename for given UUID"""
    return os.path.join(TITANIC_DIR, "%s.rep" % uuid)

# ---------------------------------------------------------------------
# Titanic request service

def titanic_request (pipe):
    worker = MajorDomoWorker("tcp://localhost:5555", "titanic.request")

    reply = None

    while True:
        # Send reply if it's not null
        # And then get next request from broker
        request = worker.recv(reply)
        if not request:
            break      # Interrupted, exit

        # Ensure message directory exists
        if not os.path.exists(TITANIC_DIR):
            os.mkdir(TITANIC_DIR)

        # Generate UUID and save message to disk
        uuid = uuid4().hex
        filename = request_filename (uuid)
        with open(filename, 'w') as f:
            pickle.dump(request, f)

        # Send UUID through to message queue
        pipe.send(uuid)

        # Now send UUID back to client
        # Done by the worker.recv() at the top of the loop
        reply = ["200", uuid]


# ---------------------------------------------------------------------
# Titanic reply service

def titanic_reply ():
    worker = MajorDomoWorker("tcp://localhost:5555", "titanic.reply")
    reply = None

    while True:
        request = worker.recv(reply)
        if not request:
            break      # Interrupted, exit

        uuid = request.pop(0)
        req_filename = request_filename(uuid)
        rep_filename = reply_filename(uuid)
        if os.path.exists(rep_filename):
            with open(rep_filename, 'r') as f:
                reply = pickle.load(f)
            reply = ["200"] + reply
        else:
            if os.path.exists(req_filename):
                reply = ["300"] # pending
            else:
                reply = ["400"] # unknown


# ---------------------------------------------------------------------
# Titanic close service

def titanic_close():
    worker = MajorDomoWorker("tcp://localhost:5555", "titanic.close")
    reply = None

    while True:
        request = worker.recv(reply)
        if not request:
            break      # Interrupted, exit

        uuid = request.pop(0)
        req_filename = request_filename(uuid)
        rep_filename = reply_filename(uuid)
        # should these be protected?  Does zfile_delete ignore files
        # that have already been removed?  That's what we are doing here.
        if os.path.exists(req_filename):
            os.remove(req_filename)
        if os.path.exists(rep_filename):
            os.remove(rep_filename)
        reply = ["200"]


def service_success(client, uuid):
    """Attempt to process a single request, return True if successful"""
    # Load request message, service will be first frame
    filename = request_filename (uuid)

    # If the client already closed request, treat as successful
    if not os.path.exists(filename):
        return True

    with open(filename, 'r') as f:
        request = pickle.load(f)
    service = request.pop(0)
    # Use MMI protocol to check if service is available
    mmi_request = [service]
    mmi_reply = client.send("mmi.service", mmi_request)
    service_ok = mmi_reply and mmi_reply[0] == "200"

    if service_ok:
        reply = client.send(service, request)
        if reply:
            filename = reply_filename (uuid)
            with open(filename, "w") as f:
                pickle.dump(reply, f)
            return True

    return False


def main():
    verbose = '-v' in sys.argv
    ctx = zmq.Context()

    # Create MDP client session with short timeout
    client = MajorDomoClient("tcp://localhost:5555", verbose)
    client.timeout = 1000 # 1 sec
    client.retries = 1 # only 1 retry

    request_pipe, peer = zpipe(ctx)
    request_thread = threading.Thread(target=titanic_request, args=(peer,))
    request_thread.daemon = True
    request_thread.start()
    reply_thread = threading.Thread(target=titanic_reply)
    reply_thread.daemon = True
    reply_thread.start()
    close_thread = threading.Thread(target=titanic_close)
    close_thread.daemon = True
    close_thread.start()

    poller = zmq.Poller()
    poller.register(request_pipe, zmq.POLLIN)
    # Main dispatcher loop
    while True:
        # Ensure message directory exists
        if not os.path.exists(TITANIC_DIR):
            os.mkdir(TITANIC_DIR)
        # We'll dispatch once per second, if there's no activity
        try:
            items = poller.poll(1000)
        except KeyboardInterrupt:
            break;              # Interrupted

        if items:

            # Append UUID to queue, prefixed with '-' for pending
            uuid = request_pipe.recv()
            with open(os.path.join(TITANIC_DIR, 'queue'), 'a') as f:
                f.write("-%s\n" % uuid)

        # Brute-force dispatcher
        #
        with open(os.path.join(TITANIC_DIR, 'queue'), 'r+b') as f:
            for entry in f.readlines():
                # UUID is prefixed with '-' if still waiting
                if entry[0] == '-':
                    uuid = entry[1:].rstrip() # rstrip '\n' etc.
                    print "I: processing request %s" % uuid
                    if service_success(client, uuid):
                        # mark queue entry as processed
                        here = f.tell()
                        f.seek(-1*len(entry), os.SEEK_CUR)
                        f.write('+')
                        f.seek(here, os.SEEK_SET)


if __name__ == '__main__':
    main()
