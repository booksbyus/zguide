# Shows how to handle Ctrl-C
require 'ffi-rzmq'

context = ZMQ::Context.new(1)
socket = context.socket(ZMQ::REP)
socket.bind("tcp://*:5558")

trap("INT") { puts "Shutting down."; socket.close; context.terminate; exit}

puts "Starting up"

while true do
  message = socket.recv_string
  puts "Message: #{message.inspect}"
  socket.send_string("Message received")
end
