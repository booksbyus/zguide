# author: Oleg Sidorov <4pcbr> i4pcbr@gmail.com
# this code is licenced under the MIT/X11 licence.
#
# Reading from multiple sockets
# This version uses a simple recv loop

require 'rubygems'
require 'ffi-rzmq'

context = ZMQ::Context.new

# Connect to task ventilator
receiver = context.socket(ZMQ::PULL)
receiver.connect('tcp://localhost:5557')

# Connect to weather server
subscriber = context.socket(ZMQ::SUB)
subscriber.connect('tcp://localhost:5556')
subscriber.setsockopt(ZMQ::SUBSCRIBE, '10001')

while true
  if receiver.recv_string(receiver_msg = '',ZMQ::NOBLOCK) && !receiver_msg.empty?
    # process task
    puts "receiver: #{receiver_msg}"
  end

  if subscriber.recv_string(subscriber_msg = '',ZMQ::NOBLOCK) && !subscriber_msg.empty?
    # process weather update
    puts "weather: #{subscriber_msg}"
  end

  # No activity, so sleep for 1 msec
  sleep 0.001
end
