#!/usr/bin/env ruby

require 'ffi-rzmq'

context = ZMQ::Context.new
subscriber = context.socket ZMQ::SUB
subscriber.connect "tcp://localhost:5563"
subscriber.setsockopt ZMQ::SUBSCRIBE, 'B'

loop do
  # Two recv s because of the multi-part message.
  address = ''
  subscriber.recv_string address

  content = ''
  subscriber.recv_string content

  puts "[#{address}] #{content}"
end
