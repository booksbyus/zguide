# Custom routing Router to Mama (ROUTER to REQ)
# Ruby version, based on the C version.
#
# While this example runs in a single process, that is just to make
# it easier to start and stop the example. Each thread has its own
# context and conceptually acts as a separate process.
#
# libzmq: 2.1.10
# ruby: 1.9.2p180 (2011-02-18 revision 30909) [i686-linux]
# ffi-rzmq: 0.9.0
#
# @author Pavel Mitin
# @email mitin.pavel@gmail.com

require 'rubygems'
require 'ffi-rzmq'

WORKER_NUMBER = 10

def receive_string(socket)
  result = ''
  socket.recv_string result
  result
end

def worker_task
  context = ZMQ::Context.new 1
  worker = context.socket ZMQ::REQ
  #  We use a string identity for ease here
  worker.setsockopt ZMQ::IDENTITY, sprintf("%04X-%04X", rand(10000), rand(10000))
  worker.connect 'ipc://routing.ipc'

  total = 0
  loop do
    # Tell the router we're ready for work
    worker.send_string 'ready'

    # Get workload from router, until finished
    workload = receive_string worker

    p "Processed: #{total} tasks" and break if workload == 'END'
    total += 1

    # Do some random work
    sleep((rand(10) + 1) / 10.0)
  end
end

context = ZMQ::Context.new 1
client = context.socket ZMQ::ROUTER
client.bind 'ipc://routing.ipc'

workers = (1..WORKER_NUMBER).map do
  Thread.new { worker_task }
end

(WORKER_NUMBER * 10).times do
  # LRU worker is next waitin in queue
  address = receive_string client
  empty = receive_string client
  ready = receive_string client

  client.send_string address, ZMQ::SNDMORE
  client.send_string '', ZMQ::SNDMORE
  client.send_string 'This is the workload'
end

# Now ask mamas to shut down and report their results
WORKER_NUMBER.times do
  address = receive_string client
  empty = receive_string client
  ready = receive_string client

  client.send_string address, ZMQ::SNDMORE
  client.send_string '', ZMQ::SNDMORE
  client.send_string 'END'
end

workers.each &:join