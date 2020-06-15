#
#  Lazy Pirate server
#  Binds REQ socket to tcp://*:5555
#  Like hwserver except:
#   - echoes request as-is
#   - randomly runs slowly, or exits to simulate a crash.
#
#   Author: Daniel Lundin <dln(at)eintr(dot)org>
#
from random import randint
import itertools
import logging
import time
import zmq

logging.basicConfig(format="%(levelname)s: %(message)s", level=logging.INFO)

context = zmq.Context()
server = context.socket(zmq.REP)
server.bind("tcp://*:5555")

for cycles in itertools.count():
    request = server.recv()

    # Simulate various problems, after a few cycles
    if cycles > 3 and randint(0, 3) == 0:
        logging.info("Simulating a crash")
        break
    elif cycles > 3 and randint(0, 3) == 0:
        logging.info("Simulating CPU overload")
        time.sleep(2)

    logging.info("Normal request (%s)", request)
    time.sleep(1)  # Do some heavy work
    server.send(request)
