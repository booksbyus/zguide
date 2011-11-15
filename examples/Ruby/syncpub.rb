#
# Synchronized publisher
#

require 'rubygems'
require 'ffi-rzmq'

# We wait for 10 subscribers
SUBSCRIBERS_EXPECTED = 10

context = ZMQ::Context.new

# Socket to talk to clients
publisher = context.socket(ZMQ::PUB)
publisher.bind("tcp://*:5561")

# Socket to receive signals
syncservice = context.socket(ZMQ::REP)
syncservice.bind("tcp://*:5562")

# Get synchronization from subscribers
puts "Waiting for subscribers"
subscribers = 0 
begin 
  # wait for synchronization request
  syncservice.recv_string('')
  # send synchronization reply
  syncservice.send_string("")
  subscribers+=1
end while subscribers < SUBSCRIBERS_EXPECTED

# Now broadcast exactly 1M updates followed by END
1000000.times do
  publisher.send_string("Rhubarb") 
end

publisher.send_string("END")
