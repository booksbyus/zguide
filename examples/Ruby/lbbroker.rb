# Load-balancing broker
# Clients and workers are shown here in-process

require 'rubygems'
require 'ffi-rzmq'

CLIENT_SIZE = 10
WORKER_SIZE = 3

def client_task(identity)
  context = ZMQ::Context.new
  client = context.socket ZMQ::REQ
  client.identity = identity
  client.connect "ipc://frontend.ipc"

  client.send_string "HELLO"
  client.recv_string reply = ""

  puts "#{identity}: #{reply}"

  client.close
  context.destroy
end

def worker_task(identity)
  context = ZMQ::Context.new
  worker = context.socket ZMQ::REQ
  worker.identity = identity
  worker.connect "ipc://backend.ipc"

  worker.send_string "READY"

  loop do
    worker.recv_string client = ""
    worker.recv_string empty = ""
    worker.recv_string request = ""

    puts "#{identity}: #{request} from #{client}"

    worker.send_strings [client, empty, "OK from #{identity}"]
  end

  worker.close
  context.destroy
end

def main_task
  context = ZMQ::Context.new
  frontend = context.socket ZMQ::ROUTER
  backend = context.socket ZMQ::ROUTER

  frontend.bind "ipc://frontend.ipc"
  backend.bind "ipc://backend.ipc"

  CLIENT_SIZE.times do |client_id|
    Thread.new { client_task "CLIENT-#{client_id}" }
  end

  WORKER_SIZE.times do |worker_id|
    Thread.new { worker_task "WORKER-#{worker_id}" }
  end

  available_workers = []
  poller = ZMQ::Poller.new
  poller.register_readable backend
  poller.register_readable frontend

  # The poller will continuously poll the backend and will poll the
  # frontend when there is at least one worker available.

  while poller.poll > 0
    poller.readables.each do |readable|
      if readable === backend
        backend.recv_string worker = ""
        backend.recv_string empty = ""
        backend.recv_strings reply = []

        frontend.send_strings reply unless reply[0] == "READY"

        # Add this worker to the list of available workers
        available_workers << worker
      elsif readable === frontend && available_workers.any?
        # Read the request from the client and forward it to the LRU worker
        frontend.recv_strings request = []
        backend.send_strings [available_workers.shift, ""] + request
      end
    end
  end

  frontend.close
  backend.close
  context.destroy
end

main_task
