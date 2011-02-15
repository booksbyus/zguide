# author: Oleg Sidorov <4pcbr> i4pcbr@gmail.com
# this code is licenced under the MIT/X11 licence.
#
# Reading from multiple sockets
# This version uses a polling

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

# Initialize a poll set
poller = ZMQ::Poller.new
poller.register(reciever, ZMQ::POLLIN)
poller.register(subscriber, ZMQ::POLLIN)

while true
  poller.poll(:blocking)
  poller.readables.each do |socket|
    if socket === reciever
      message = socket.recv_string
      # process task
    elsif socket === subscriber
      message = socket.recv_string
      # process weather update
    end
  end
end
