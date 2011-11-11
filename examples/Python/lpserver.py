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
import time
import zmq

context = zmq.Context(1)
server = context.socket(zmq.REP)
server.bind("tcp://*:5555")

cycles = 0
while True:
    request = server.recv()
    cycles += 1

    # Simulate various problems, after a few cycles
    if cycles > 3 and randint(0, 3) == 0:
        print "I: Simulating a crash"
        break
    elif cycles > 3 and randint(0, 3) == 0:
        print "I: Simulating CPU overload"
        time.sleep(2)

    print "I: Normal request (%s)" % request
    time.sleep(1) # Do some heavy work
    server.send(request)

server.close()
context.term()
