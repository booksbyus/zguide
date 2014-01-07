#
#   Shows how to handle Ctrl-C
#
import signal
import time
import zmq

interrupted = False

def signal_handler(signum, frame):
    print("W: custom interrupt handler called.")

context = zmq.Context()
socket = context.socket(zmq.REP)
socket.bind("tcp://*:5558")

# SIGINT will normally raise a KeyboardInterrupt, just like any other Python call
try:
    socket.recv()
except KeyboardInterrupt:
    print("W: interrupt received, proceeding...")

# or you can use a custom handler,
# in which case recv will fail with EINTR
signal.signal(signal.SIGINT, signal_handler)
try:
    message = socket.recv()
except zmq.ZMQError as e:
    print("W: recv failed with: %s" % e)
