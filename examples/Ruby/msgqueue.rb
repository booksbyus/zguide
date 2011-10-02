#
# Simple message queuing broke
# Same as request-reply broker but using QUEUE device
#

require 'rubygems'
require 'ffi-rzmq'

context = ZMQ::Context.new

# Socket facing clients
frontend = context.socket(ZMQ::XREP)
frontend.bind('tcp://*:5559')

# Socket facing services
backend = context.socket(ZMQ::XREQ)
backend.bind('tcp://*:5560')

# Start built-in device
poller = ZMQ::Device.new(ZMQ::QUEUE,frontend,backend)
