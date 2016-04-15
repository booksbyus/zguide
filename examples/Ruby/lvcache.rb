#!/usr/bin/env ruby

#
# Last value cache
# Uses XPUB subscription messages to re-send data
#
require 'ffi-rzmq'
context = ZMQ::Context.new

frontend = context.socket(ZMQ::SUB)
frontend.connect("tcp://*:5557")
backend = context.socket(ZMQ::XPUB)
backend.bind("tcp://*:5558")

# Subscribe to every single topic from publisher
frontend.setsockopt(ZMQ::SUBSCRIBE, "")

# Store last instance of each topic in a cache
cache = {}

# We route topic updates from frontend to backend, and we handle subscriptions
# by sending whatever we cached, if anything:
poller = ZMQ::Poller.new
[frontend, backend].each { |sock| poller.register_readable sock }
loop do
  poller.poll(1000)
  poller.readables.each do |sock|
    if sock == frontend
      # Any new topic data we cache and then forward
      frontend.recv_strings(parts = [])
      topic, data = parts
      cache[topic] = data
      backend.send_strings(parts)
    elsif sock == backend
      # When we get a new subscription we pull data from the cache:
      backend.recv_strings(parts = [])
      event, _ = parts
      # Event is one byte 0=unsub or 1=sub, followed by topic
      if event[0].ord == 1
        topic = event[1..-1]
        puts "Sending cached topic #{topic}"
        previous = cache[topic]
        backend.send_strings([topic, previous]) if previous
      end
    end
  end
end
