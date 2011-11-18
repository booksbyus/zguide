# Weather proxy device
#
# Author: Lev Givon <lev(at)columbia(dot)edu>

import zmq

context = zmq.Context()

# This is where the weather server sits
frontend = context.socket(zmq.SUB)
frontend.connect("tcp://192.168.55.210:5556")

# This is our public endpoint for subscribers
backend = context.socket(zmq.PUB)
backend.bind("tcp://10.1.1.0:8100")

# Subscribe on everything
frontend.setsockopt(zmq.SUBSCRIBE, '')

# Shunt messages out to our own subscribers
while True:
    # Process all parts of the message
    message = frontend.recv()
    more = frontend.getsockopt(zmq.RCVMORE)
    if more:
        backend.send(message, zmq.SNDMORE)
    else:
        backend.send(message)  # last message part
