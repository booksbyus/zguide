#
#   Shows how to handle Ctrl-C
#
import zmq
import signal

interrupted = False

def signal_handler(signum, frame):
    global interrupted
    interrupted = True

context = zmq.Context()
socket = context.socket(zmq.REP)
socket.bind("tcp://*:5558")

# SIGINT will normally raise a KeyboardInterrupt, just like any other Python call
try:
    socket.recv()
except KeyboardInterrupt:
    print "W: interrupt received, proceeding..."

# or you can use a custom handler
counter = 0
signal.signal(signal.SIGINT, signal_handler)
while True:
    try:
        message = socket.recv(zmq.NOBLOCK)
    except zmq.ZMQError:
        pass
    counter += 1
    if interrupted:
        print "W: interrupt received, killing server..."
        break
