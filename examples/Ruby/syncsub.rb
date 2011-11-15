#
# Synchronized subscriber
#

require 'rubygems'
require 'ffi-rzmq'

context = ZMQ::Context.new

# First, connect our subscriber socket
subscriber = context.socket(ZMQ::SUB)
subscriber.connect("tcp://localhost:5561")
subscriber.setsockopt(ZMQ::SUBSCRIBE,"")

# 0MQ is so fast, we need to wait a while...
sleep(1)

# Second, synchronize with publisher
synclient = context.socket(ZMQ::REQ)
synclient.connect("tcp://localhost:5562")

# - send a synchronization request
synclient.send_string("")

# - wait for synchronization reply
synclient.recv_string('')

# Third, get our updates and report how many we got
update_nbr=0
loop do 
  subscriber.recv_string(string = '')
  break if string == "END"
  update_nbr+=1
end

puts "Received #{update_nbr} updates"
