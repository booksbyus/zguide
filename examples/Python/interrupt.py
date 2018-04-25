#
#   Shows how to handle Ctrl-C
#
import signal
import time
import zmq

context = zmq.Context()
socket = context.socket(zmq.REP)
socket.bind("tcp://*:5558")

# SIGINT will normally raise a KeyboardInterrupt, just like any other Python call
try:
    socket.recv()
except KeyboardInterrupt:
    print("W: interrupt received, stopping...")
finally:
    # clean up
    socket.close()
    context.term()
