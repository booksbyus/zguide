# author: Oleg Sidorov <4pcbr> i4pcbr@gmail.com
# this code is licenced under the MIT/X11 licence.

require 'rubygems'
require 'ffi-rzmq'

context = ZMQ::Context.new(1)
socket = context.socket(ZMQ::REP)
socket.connect('tcp://localhost:5560')

while true
  message = socket.recv_string
  puts "Recieved request: #{message}"
  socket.send_string('World')
end