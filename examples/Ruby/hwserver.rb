# author: Bill Desmarais bill@witsaid.com
# this code is licenced under the MIT/X11 licence.

require 'rubygems'
require 'ffi-rzmq'

context = ZMQ::Context.new(1)

puts "Starting Hello World server..."

# socket to listen for clients
socket = context.socket(ZMQ::REP)
socket.bind("tcp://*:5555")

while true do
  # Wait for next request from client
  request = ''
  rc = socket.recv_string(request)

  puts "Received request. Data: #{request.inspect}"

  # Do some 'work'
  sleep 1

  # Send reply back to client
  socket.send_string("world")

end
