# author: Oleg Sidorov <4pcbr> i4pcbr@gmail.com
# this code is licenced under the MIT/X11 licence.

require 'rubygems'
require 'ffi-rzmq'

context = ZMQ::Context.new
frontend = context.socket(ZMQ::ROUTER)
backend = context.socket(ZMQ::DEALER)

frontend.bind('tcp://*:5559')
backend.bind('tcp://*:5560')

poller = ZMQ::Poller.new
poller.register(frontend, ZMQ::POLLIN)
poller.register(backend, ZMQ::POLLIN)

loop do
  poller.poll(:blocking)
  poller.readables.each do |socket|
    if socket === frontend
      socket.recv_strings(messages = [])
      backend.send_strings(messages)
    elsif socket === backend
      socket.recv_strings(messages = [])
      frontend.send_strings(messages)
    end
  end
end
