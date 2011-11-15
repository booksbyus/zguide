# author: Oleg Sidorov <4pcbr> i4pcbr@gmail.com
# this code is licenced under the MIT/X11 licence.
#
# Reading from multiple sockets
# This version uses a polling

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

# Initialize a poll set
poller = ZMQ::Poller.new
poller.register(receiver, ZMQ::POLLIN)
poller.register(subscriber, ZMQ::POLLIN)

while true
  poller.poll(:blocking)
  poller.readables.each do |socket|
    if socket === receiver
      socket.recv_string(message = '')
      # process task
       puts "task: #{message}"
      
    elsif socket === subscriber
      socket.recv_string(message = '')
      # process weather update
      puts "weather: #{message}"
      
    end
  end
end
