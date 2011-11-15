# author: Oleg Sidorov <4pcbr> i4pcbr@gmail.com
# this code is licenced under the MIT/X11 licence.

require 'rubygems'
require 'ffi-rzmq'

context = ZMQ::Context.new
socket = context.socket(ZMQ::REP)
socket.connect('tcp://localhost:5560')

loop do
  socket.recv_string(message = '')
  puts "Received request: #{message}"
  socket.send_string('World')
end
