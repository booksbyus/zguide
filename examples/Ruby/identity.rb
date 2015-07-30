#!/usr/bin/env ruby

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
anon_message = ZMQ::Message.new("ROUTER uses a generated 5 byte identity")
anonymous.sendmsg(anon_message)
s_dump(sink)

# Set the identity ourselves
identified = context.socket(ZMQ::DEALER)
identified.setsockopt(ZMQ::IDENTITY, "PEER2")
identified.connect(uri)
identified_message = ZMQ::Message.new("Router uses socket identity")
identified.sendmsg(identified_message)
s_dump(sink)
