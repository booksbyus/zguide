# Broker peering simulation (part 2)
# Prototypes the request-reply flow
#
# Translated from C by Devin Christensen: http://github.com/devin-c

require "rubygems"
require "ffi-rzmq"

NUMBER_OF_CIENTS = 10
NUMBER_OF_WORKERS = 3
WORKER_READY = "\x01"

class Client
  def initialize(broker_name)
    @context = ZMQ::Context.new
    @socket = @context.socket ZMQ::REQ
    @socket.connect "ipc://#{broker_name}-localfe.ipc"
  end

  def run
    loop do
      break if @socket.send_string("HELLO") == -1
      break if @socket.recv_string(reply = "") == -1
      puts "Client: #{reply}"
      sleep 1
    end

    @socket.close
    @context.terminate
  end
end

class Worker
  def initialize(broker_name)
    @context = ZMQ::Context.new
    @socket = @context.socket ZMQ::REQ
    @socket.connect "ipc://#{broker_name}-localbe.ipc"
  end

  def run
    @socket.send_string WORKER_READY

    loop do
      break if @socket.recv_strings(frames = []) == -1
      puts "Worker: #{frames.last}"
      break if @socket.send_strings(frames[0..-2] + ["OK"]) == -1
    end

    @socket.close
    @context.terminate
  end
end

class Broker
  attr_reader :name

  def initialize(name, peers)
    raise ArgumentError, "A broker require's a name" unless name
    raise ArgumentError, "A broker require's peers" unless peers.any?

    puts "I: preparing broker at #{name}..."

    @name = name
    @peers = peers
    @context = ZMQ::Context.new
    @available_workers = []

    setup_cloud_backend
    setup_cloud_frontend
    setup_local_backend
    setup_local_frontend
  end

  def run
    poller = ZMQ::Poller.new

    poller.register_readable @cloud_backend
    poller.register_readable @local_backend

    poller.register_readable @cloud_frontend
    poller.register_readable @local_frontend

    while poller.poll > 0
      poller.readables.each do |readable|
        if @available_workers.any?
          if readable === @local_frontend
            @local_frontend.recv_strings frames = []
            route_to_backend frames, true
          elsif readable === @cloud_frontend
            @cloud_frontend.recv_strings frames = []
            route_to_backend frames, false
          end
        else
          if readable === @local_backend
            @local_backend.recv_strings frames = []
            @available_workers << frames.shift(2)[0]

            route_to_frontend(frames) unless frames == [WORKER_READY]
          elsif readable === @cloud_backend
            @cloud_backend.recv_strings frames = []

            route_to_frontend frames[2..-1]
          end
        end
      end
    end

    @cloud_backend.close
    @local_backend.close
    @cloud_frontend.close
    @local_frontend.close
    @context.terminate
  end

  private
  def route_to_frontend(frames)
    if @peers.include? frames[0]
      @cloud_frontend.send_strings frames
    else
      @local_frontend.send_strings frames
    end
  end

  def route_to_backend(frames, reroutable = false)
    if reroutable && rand(5) == 0
      @cloud_backend.send_strings [@peers.sample, ""] + frames
    else
      @local_backend.send_strings [@available_workers.shift, ""] + frames
    end
  end

  def setup_cloud_backend
    @cloud_backend = @context.socket ZMQ::ROUTER
    @cloud_backend.identity = @name

    @peers.each do |peer|
      puts "I: connecting to cloud frontend at #{peer}"
      @cloud_backend.connect "ipc://#{peer}-cloud.ipc"
    end
  end

  def setup_cloud_frontend
    @cloud_frontend = @context.socket ZMQ::ROUTER
    @cloud_frontend.identity = @name
    @cloud_frontend.bind "ipc://#{@name}-cloud.ipc"
  end

  def setup_local_backend
    @local_backend = @context.socket ZMQ::ROUTER
    @local_backend.bind "ipc://#{@name}-localbe.ipc"
  end

  def setup_local_frontend
    @local_frontend = @context.socket ZMQ::ROUTER
    @local_frontend.bind "ipc://#{@name}-localfe.ipc"
  end
end

begin
  broker = Broker.new(ARGV.shift, ARGV)

  puts "Press Enter when all the brokers are started: "

  STDIN.getc

  NUMBER_OF_WORKERS.times do
    Thread.new { Worker.new(broker.name).run }
  end

  NUMBER_OF_CIENTS.times do
    Thread.new { Client.new(broker.name).run }
  end

  broker.run

rescue ArgumentError
  puts "usage: ruby peering2.rb broker_name [peer_name ...]"
end
