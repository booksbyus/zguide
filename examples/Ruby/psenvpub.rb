#! /usr/bin/env ruby
require 'zmq'
context = ZMQ::Context.new
publisher = context.socket ZMQ::PUB
publisher.bind "tcp://*:5563"
while true
  publisher.send 'A',ZMQ::SNDMORE
  publisher.send "We don't want to see this."

  publisher.send 'B',ZMQ::SNDMORE
  publisher.send "We would like to see this."

  sleep 1
end
publisher.close
