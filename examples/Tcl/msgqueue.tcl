#
#  Simple message queuing broker
#  Same as request-reply broker but using QUEUE device
#

package require zmq

zmq context context

#  Socket facing clients
zmq socket frontend context ROUTER
frontend bind "tcp://*:5559"

#  Socket facing services
zmq socket backend context DEALER
backend bind "tcp://*:5560"

#  Start built-in device
zmq device QUEUE frontend backend

#  We never get here…
frontend close
backend close
context term

