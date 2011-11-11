#! /usr/bin/env ruby
require 'zmq'
context = ZMQ::Context.new
subscriber = context.socket ZMQ::SUB
subscriber.connect "tcp://*:5563"
subscriber.setsockopt ZMQ::SUBSCRIBE,'B'
while true
  # Two recv s because of the multi-part message.
  address = subscriber.recv
  content = subscriber.recv
  puts "[#{address}] #{content}"
end
