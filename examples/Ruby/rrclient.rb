# author: Oleg Sidorov <4pcbr> i4pcbr@gmail.com
# this code is licenced under the MIT/X11 licence.

require 'rubygems'
require 'ffi-rzmq'

context = ZMQ::Context.new
socket = context.socket(ZMQ::REQ)
socket.connect('tcp://localhost:5559')

10.times do |request|
  string = "Hello #{request}"
  socket.send_string(string)
  puts "Sending string [#{string}]"
  message = socket.recv_string
  puts "Recieved reply #{request}[#{message}]"
end
