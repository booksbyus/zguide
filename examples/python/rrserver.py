#
#   Request-reply service in Python
#   Connects REP socket to tcp://*:5560
#   Expects "Hello" from client, replies with "World"
#
import zmq
import time

context = zmq.Context()
socket = context.socket(zmq.REP)
socket.connect("tcp://localhost:5560")

while True:
    message = socket.recv()
    print "Received request: ", message
    socket.send("World")
