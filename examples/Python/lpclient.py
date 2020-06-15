#
#  Lazy Pirate client
#  Use zmq_poll to do a safe request-reply
#  To run, start lpserver and then randomly kill/restart it
#
#   Author: Daniel Lundin <dln(at)eintr(dot)org>
#
import logging
import zmq

logging.basicConfig(level=logging.INFO)

REQUEST_TIMEOUT = 2500
REQUEST_RETRIES = 3
SERVER_ENDPOINT = "tcp://localhost:5555"

context = zmq.Context()

logging.info("Connecting to server…")
client = context.socket(zmq.REQ)
client.connect(SERVER_ENDPOINT)

sequence = 0
retries_left = REQUEST_RETRIES
while retries_left > 0:
    sequence += 1
    request = str(sequence).encode()
    logging.info("Sending (%s)", request)
    client.send(request)

    while True:
        if client.poll(REQUEST_TIMEOUT) == zmq.POLLIN:
            reply = client.recv()
            if int(reply) == sequence:
                logging.info("Server replied OK (%s)", reply)
                retries_left = REQUEST_RETRIES
            else:
                logging.error("Malformed reply from server: %s", reply)

            break

        logging.warning("No response from server, retrying…")
        # Socket is confused. Close and remove it.
        client.setsockopt(zmq.LINGER, 0)
        client.close()
        retries_left -= 1
        if retries_left == 0:
            logging.error("Server seems to be offline, abandoning")
            break

        logging.info("Reconnecting and resending (%s)", request)
        # Create new connection
        client = context.socket(zmq.REQ)
        client.connect(SERVER_ENDPOINT)
        client.send(request)
