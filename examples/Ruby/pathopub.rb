#!/usr/bin/env ruby

#
# Pathological publisher
# Sends out 1,000 topics and then one random update per second
#
require 'ffi-rzmq'
context = ZMQ::Context.new

TOPIC_COUNT = 1_000

publisher = context.socket(ZMQ::PUB)
if ARGV[0]
  publisher.bind(ARGV[0])
else
  publisher.bind("tcp://*:5556")
end

# Ensure subscriber connection has time to complete
sleep 1

TOPIC_COUNT.times do |n|
  topic = "%03d" % [n]
  publisher.send_strings([topic, "Save Roger"])
end

loop do
  sleep 1
  topic = "%03d" % [rand(1000)]
  publisher.send_strings([topic, "Off with his head!"])
end
