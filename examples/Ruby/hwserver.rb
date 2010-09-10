require 'rubygems'
require 'ffi-rzmq'

<<<<<<< HEAD
To submit a new translation email it to 1000 4 20 24 25 29 30 44 46 107 109 114 121 1000EMAIL).  Please:

* Stick to identical functionality and naming used in examples so that readers
  can easily compare languages.
* You MUST place your name as author in the examples so readers can contact you.
* You MUST state in the email that you license your code under the MIT/X11
  license.

Subscribe to this list at http://lists.zeromq.org/mailman/listinfo/zeromq-dev.
=======
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
>>>>>>> mlc/master
