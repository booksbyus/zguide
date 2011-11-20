require 'rubygems'
require 'ffi-rzmq'

def task(name, context)
  worker = context.socket ZMQ::DEALER
  worker.setsockopt ZMQ::IDENTITY, name
  worker.connect "tcp://127.0.0.1:9000"

  total = 0
  loop do
    data = ''
    worker.recv_string data
    p "#{name} received: #{total}" and break if data == 'END'
    total += 1
  end 
end

context = ZMQ::Context.new 1
client = context.socket ZMQ::ROUTER
client.bind "tcp://127.0.0.1:9000"

worker_1 = Thread.new { task 'A', context }
worker_2 = Thread.new { task 'B', context }

sleep 1

10.times do
  address = rand(3) % 3 == 0 ? 'A' : 'B'
  client.send_string address, ZMQ::SNDMORE
  client.send_string "This is the workload [#{address}]"
end

%w(A B).each do |address|
  client.send_string address, ZMQ::SNDMORE
  client.send_string 'END'
end

worker_1.join
worker_2.join
