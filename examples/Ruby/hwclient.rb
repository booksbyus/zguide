require 'rubygems'
require 'ffi-rzmq'

context = ZMQ::Context.new(1)

# Socket to talk to server
puts "Connecting to hello world server..."
requester = context.socket(ZMQ::REQ)
requester.connect("tcp://localhost:5555")

0.upto(9) do |request_nbr|
  puts "Sending request #{request_nbr}..."
  requester.send_string "Hello"

  reply = requester.recv_string
  puts "Received reply #{request_nbr}: [#{reply}]"
end
