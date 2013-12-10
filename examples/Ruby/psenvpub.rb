#!/usr/bin/env ruby

require 'ffi-rzmq'

context = ZMQ::Context.new
publisher = context.socket ZMQ::PUB
publisher.bind "tcp://*:5563"

loop do
  publisher.send_string 'A', ZMQ::SNDMORE
  publisher.send_string "We don't want to see this."

  publisher.send_string 'B', ZMQ::SNDMORE
  publisher.send_string "We would like to see this."

  sleep 1
end

publisher.close
