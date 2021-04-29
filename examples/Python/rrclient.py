#
#   Request-reply client in Python
#   Connects REQ socket to tcp://localhost:5559
#   Sends "Hello" to server, expects "World" back
#
import zmq

#  Prepare our context and sockets
context = zmq.Context()
socket = context.socket(zmq.REQ)
socket.connect("tcp://localhost:5559")

#  Do 10 requests, waiting each time for a response
for request in range(1, 11):
    socket.send(b"Hello")
    message = socket.recv()
    print(f"Received reply {request} [{message}]")
