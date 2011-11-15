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
