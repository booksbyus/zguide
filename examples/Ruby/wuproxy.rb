# 
# Weather proxy device
#

require "rubygems"
require 'ffi-rzmq'

context = ZMQ::Context.new(1)

# This is where the weather server sits
frontend  = context.socket(ZMQ::SUB)
frontend.connect("tcp://192.168.55.210:5556")

# This is our public endpoint for subscribers
backend = context.socket(ZMQ::PUB) 
backend.bind("tcp://10.1.1.0:8100")
# Subscribe on everything
frontend.setsockopt(ZMQ::SUBSCRIBE,"")

loop do 
  loop do
    # Process all parts of the message
    message = ZMQ::Message.new
    frontend.recv(message)
    more=frontend.getsockopt(ZMQ::RCVMORE)
    backend.send(message, more ? ZMQ::SNDMORE : 0 )
    break unless more # Last message part
  end
end
