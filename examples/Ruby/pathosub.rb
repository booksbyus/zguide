#!/usr/bin/env ruby

#
# Pathological subscriber
# Subscribes to one random topic and prints received messages
#
require 'ffi-rzmq'
context = ZMQ::Context.new

subscriber = context.socket(ZMQ::SUB)
subscriber.connect(ARGV[0] || "tcp://localhost:5556")

topic = "%03d" % [rand(1000)]
subscriber.setsockopt(ZMQ::SUBSCRIBE, topic)

loop do
  subscriber.recv_strings(parts = [])
  topic, data = parts
  puts "#{topic}: #{data}"
end
