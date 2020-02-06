"""
Titanic service

Implements server side of http:#rfc.zeromq.org/spec:9

Author: Min RK <benjaminrk@gmail.com>
"""

import pickle
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

def request_filename (suuid):
    """Returns freshly allocated request filename for given UUID str"""
    return os.path.join(TITANIC_DIR, "%s.req" % suuid)

#

def reply_filename (suuid):
    """Returns freshly allocated reply filename for given UUID str"""
    return os.path.join(TITANIC_DIR, "%s.rep" % suuid)

# ---------------------------------------------------------------------
# Titanic request service

def titanic_request (pipe):
    worker = MajorDomoWorker("tcp://localhost:5555", b"titanic.request")

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
        suuid = uuid4().hex
        filename = request_filename (suuid)
        with open(filename, 'wb') as f:
            pickle.dump(request, f)

        # Send UUID through to message queue
        pipe.send_string(suuid)

        # Now send UUID back to client
        # Done by the worker.recv() at the top of the loop
        reply = [b"200", suuid.encode('utf-8')]


# ---------------------------------------------------------------------
# Titanic reply service

def titanic_reply ():
    worker = MajorDomoWorker("tcp://localhost:5555", b"titanic.reply")
    reply = None

    while True:
        request = worker.recv(reply)
        if not request:
            break      # Interrupted, exit

        suuid = request.pop(0).decode('utf-8')
        req_filename = request_filename(suuid)
        rep_filename = reply_filename(suuid)
        if os.path.exists(rep_filename):
            with open(rep_filename, 'rb') as f:
                reply = pickle.load(f)
            reply = [b"200"] + reply
        else:
            if os.path.exists(req_filename):
                reply = [b"300"] # pending
            else:
                reply = [b"400"] # unknown


# ---------------------------------------------------------------------
# Titanic close service

def titanic_close():
    worker = MajorDomoWorker("tcp://localhost:5555", b"titanic.close")
    reply = None

    while True:
        request = worker.recv(reply)
        if not request:
            break      # Interrupted, exit

        suuid = request.pop(0).decode('utf-8')
        req_filename = request_filename(suuid)
        rep_filename = reply_filename(suuid)
        # should these be protected?  Does zfile_delete ignore files
        # that have already been removed?  That's what we are doing here.
        if os.path.exists(req_filename):
            os.remove(req_filename)
        if os.path.exists(rep_filename):
            os.remove(rep_filename)
        reply = [b"200"]


def service_success(client, suuid):
    """Attempt to process a single request, return True if successful"""
    # Load request message, service will be first frame
    filename = request_filename (suuid)

    # If the client already closed request, treat as successful
    if not os.path.exists(filename):
        return True

    with open(filename, 'rb') as f:
        request = pickle.load(f)
    service = request.pop(0)
    # Use MMI protocol to check if service is available
    mmi_request = [service]
    mmi_reply = client.send(b"mmi.service", mmi_request)
    service_ok = mmi_reply and mmi_reply[0] == b"200"

    if service_ok:
        reply = client.send(service, request)
        if reply:
            filename = reply_filename (suuid)
            with open(filename, "wb") as f:
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

    queue_filename = os.path.join(TITANIC_DIR, 'queue')

    # Main dispatcher loop
    while True:
        # Ensure message directory exists
        if not os.path.exists(TITANIC_DIR):
            os.mkdir(TITANIC_DIR)
            f = open(queue_filename,'wb')
            f.close()
        # We'll dispatch once per second, if there's no activity
        try:
            items = poller.poll(1000)
        except KeyboardInterrupt:
            break;              # Interrupted

        if items:
            # Append UUID to queue, prefixed with '-' for pending
            suuid = request_pipe.recv().decode('utf-8')
            with open(queue_filename, 'ab') as f:
                line = "-%s\n" % suuid
                f.write(line.encode('utf-8'))

        # Brute-force dispatcher
        with open(queue_filename, 'rb+') as f:
            for entry in f.readlines():
                entry = entry.decode('utf-8')
                # UUID is prefixed with '-' if still waiting
                if entry[0] == '-':
                    suuid = entry[1:].rstrip() # rstrip '\n' etc.
                    print ("I: processing request %s" % suuid)
                    if service_success(client, suuid):
                        # mark queue entry as processed
                        here = f.tell()
                        f.seek(-1*len(entry), os.SEEK_CUR)
                        f.write('+'.encode('utf-8'))
                        f.seek(here, os.SEEK_SET)


if __name__ == '__main__':
    main()
