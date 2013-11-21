#
#   Weather update server
#   Binds PUB socket to tcp://*:5556
#   Publishes random weather updates
#

import zmq
from random import randrange


context = zmq.Context()
socket = context.socket(zmq.PUB)
socket.bind("tcp://*:5556")

while True:
    zipcode = randrange(1, 100000)
    temperature = randrange(1, 215) - 80
    relhumidity = randrange(1, 50) + 10

    socket.send("%d %d %d" % (zipcode, temperature, relhumidity))
