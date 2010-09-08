require 'rubygems'
require 'ffi-rzmq'

context = ZMQ::Context.new(1)

# Socket to talk to clients
responder = context.socket(ZMQ::REP)
responder.bind('tcp://*:5555')

while true
  # Wait for next request from client
  message = responder.recv_string
  print "Received request: #{message}\n"

  # Do some 'work'
  sleep 1

  # Send reply back to client
  responder.send_string("World")
end
