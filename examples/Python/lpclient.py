#
#  Lazy Pirate client
#  Use zmq_poll to do a safe request-reply
#  To run, start lpserver and then randomly kill/restart it
#
#   Author: Daniel Lundin <dln(at)eintr(dot)org>
#
import itertools
import logging
import sys
import zmq

logging.basicConfig(level=logging.INFO)

REQUEST_TIMEOUT = 2500
REQUEST_RETRIES = 3
SERVER_ENDPOINT = "tcp://localhost:5555"

context = zmq.Context()

logging.info("Connecting to server…")
client = context.socket(zmq.REQ)
client.connect(SERVER_ENDPOINT)

for sequence in itertools.count():
    request = str(sequence).encode()
    logging.info("Sending (%s)", request)
    client.send(request)

    for retry_count in range(REQUEST_RETRIES):
        if client.poll(REQUEST_TIMEOUT) == zmq.POLLIN:
            reply = client.recv()
            if int(reply) == sequence:
                logging.info("Server replied OK (%s)", reply)
                break
            else:
                logging.error("Malformed reply from server: %s", reply)
                continue

        logging.warning("No response from server, retrying…")
        # Socket is confused. Close and remove it.
        client.setsockopt(zmq.LINGER, 0)
        client.close()
        if retry_count == (REQUEST_RETRIES - 1):
            logging.error("Server seems to be offline, abandoning")
            sys.exit()

        logging.info("Reconnecting and resending (%s)", request)
        # Create new connection
        client = context.socket(zmq.REQ)
        client.connect(SERVER_ENDPOINT)
        client.send(request)
