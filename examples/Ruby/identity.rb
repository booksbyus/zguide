#
#
#   Identity check in Ruby
#
#
require 'ffi-rzmq'
require './zhelpers.rb'

context = ZMQ::Context.new
uri = "inproc://example"

sink = context.socket(ZMQ::ROUTER)
sink.bind(uri)

# 0MQ will set the identity here
anonymous = context.socket(ZMQ::DEALER)
anonymous.connect(uri)
anon_message = ZMQ::Message.new("Router uses a generated UUID")
anonymous.send(anon_message)
s_dump(sink)

# 0MQ will set the identity here
identified = context.socket(ZMQ::DEALER)
identified.setsockopt(ZMQ::IDENTITY, "Hello")
identified.connect(uri)
identified_message = ZMQ::Message.new("Router uses socket identity")
identified.send(identified_message)
s_dump(sink)
