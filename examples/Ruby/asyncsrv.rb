# Asynchronous client-to-server (DEALER to ROUTER)

require 'rubygems'
require 'ffi-rzmq'

def client
  context = ZMQ::Context.new
  client = context.socket ZMQ::DEALER
  client.identity = "%04X-%04X" % [rand(0x10000), rand(0x10000)]
  client.connect "ipc://frontend.ipc"

  poller = ZMQ::Poller.new
  poller.register_readable(client)

  request_number = 0

  loop do

    100.times do |tick|
      if poller.poll(10) == 1
        client.recv_strings message = []
        puts "#{client.identity}: #{message.last}"
      end
    end

    client.send_string "Req ##{request_number += 1}"
  end

  client.close
  context.destroy
end

def worker(context)
  worker = context.socket ZMQ::DEALER
  worker.connect "inproc://backend"

  loop do
    worker.recv_strings message = []

    rand(0..4).times do
      sleep rand
      worker.send_strings message
    end
  end

  worker.close
end

def server
  context = ZMQ::Context.new
  frontend = context.socket ZMQ::ROUTER
  backend = context.socket ZMQ::DEALER

  frontend.bind "ipc://frontend.ipc"
  backend.bind "inproc://backend"

  poller = ZMQ::Poller.new
  poller.register_readable frontend
  poller.register_readable backend

  5.times { Thread.new { worker context } }

  ZMQ::Device.create ZMQ::QUEUE, frontend, backend

end

3.times { Thread.new { client } }

server
