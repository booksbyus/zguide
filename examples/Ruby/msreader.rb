# author: Oleg Sidorov <4pcbr> i4pcbr@gmail.com
# this code is licenced under the MIT/X11 licence.
#
# Reading from multiple sockets
# This version uses a simple recv loop

require 'rubygems'
require 'ffi-rzmq'

context = ZMQ::Context.new

# Connect to task ventilator
reciever = context.socket(ZMQ::PULL)
reciever.connect('tcp://localhost:5557')

# Connect to weather server
subscriber = context.socket(ZMQ::SUB)
subscriber.connect('tcp://localhost:5556')
subscriber.setsockopt(ZMQ::SUBSCRIBE, '10001')

while true
  if reciever_msg = reciever.recv_string(ZMQ::NOBLOCK) && !reciever_msg.empty?
    # process task
  end

  if subscriber_msg = subscriber.recv_string(ZMQ::NOBLOCK) && !subscriber_msg.empty?
    # process weather update
  end

  # No activity, so sleep for 1 msec
  sleep 0.001
end