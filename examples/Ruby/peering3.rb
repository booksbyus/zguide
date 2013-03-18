# Broker peering simulation (part 3)
# Prototypes the full flow of status and tasks
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
    @frontend = @context.socket ZMQ::REQ
    @monitor = @context.socket ZMQ::PUSH
    @frontend.connect "ipc://#{broker_name}-localfe.ipc"
    @monitor.connect "ipc://#{broker_name}-monitor.ipc"
  end

  def run
    poller = ZMQ::Poller.new
    poller.register_readable @frontend

    catch(:exit) do
      loop do
        sleep rand 5

        rand(15).times do
          task_id = "%04X" % rand(0x10000)

          @frontend.send_string task_id

          if poller.poll(10_000) == 1
            @frontend.recv_string reply = ""
            throw :exit unless reply == task_id
            @monitor.send_string "#{reply}"
          else
            @monitor.send_string "E:CLIENT EXIT - lost task #{task_id}"
            throw :exit
          end
        end
      end
    end

    @frontend.close
    @monitor.close
    @context.terminate
  end
end

class Worker
  def initialize(broker_name)
    @context = ZMQ::Context.new
    @backend = @context.socket ZMQ::REQ
    @backend.connect "ipc://#{broker_name}-localbe.ipc"
  end

  def run
    @backend.send_string WORKER_READY

    loop do
      @backend.recv_strings frames = []
      sleep rand 2 # Sleep either 0 or 1 second
      @backend.send_strings frames
    end

    @backend.close
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
    @peers_capacity = {}

    setup_cloud_backend
    setup_cloud_frontend
    setup_local_backend
    setup_local_frontend
    setup_state_frontend
    setup_state_backend
    setup_monitor
  end

  def run
    poller = ZMQ::Poller.new

    poller.register_readable @cloud_backend
    poller.register_readable @cloud_frontend
    poller.register_readable @local_backend
    poller.register_readable @local_frontend
    poller.register_readable @state_frontend
    poller.register_readable @monitor

    while poller.poll > 0
      cached_local_capacity = @available_workers.size

      poller.readables.each do |readable|
        case readable
        when @local_frontend

          # Route local tasks to local or cloud workers
          if total_capacity > 0
            @local_frontend.recv_strings frames = []
            route_to_backend frames
          end

        when @cloud_frontend

          # Route tasks from the cloud to local workers only
          if @available_workers.any?
            @cloud_frontend.recv_strings frames = []
            route_to_backend frames
          end

        when @local_backend
          @local_backend.recv_strings frames = []
          @available_workers << frames.shift(2)[0]

          route_to_frontend(frames) unless frames == [WORKER_READY]

        when @cloud_backend
          @cloud_backend.recv_strings frames = []

          route_to_frontend frames[2..-1]

        when @state_frontend
          @state_frontend.recv_string peer = ""
          @state_frontend.recv_string capacity = ""
          @peers_capacity[peer] = capacity.to_i

        when @monitor
          @monitor.recv_string message = ""
          puts message
        end
      end

      unless cached_local_capacity == @available_workers.size
        @state_backend.send_strings [@name, @available_workers.size.to_s]
      end
    end

    @cloud_backend.close
    @local_backend.close
    @cloud_frontend.close
    @local_frontend.close
    @context.terminate
  end

  private
  def total_capacity
    cloud_capacity = @peers_capacity.reduce(0) do |sum, (peer, capacity)|
      sum + capacity
    end

    cloud_capacity + @available_workers.size
  end

  def route_to_backend(frames)

    # Route to local workers whenever they're available
    if @available_workers.any?
      @local_backend.send_strings [@available_workers.shift, ""] + frames

    # When there are no local workers available, route to the peer with
    # the greatest capacity
    else
      peer = @peers_capacity.max_by { |x| x[1] }[0]
      @cloud_backend.send_strings [peer, ""] + frames
    end

  def route_to_frontend(frames)
    if @peers.include? frames[0]
      @cloud_frontend.send_strings frames
    else
      @local_frontend.send_strings frames
    end
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

  def setup_monitor
    @monitor = @context.socket ZMQ::PULL
    @monitor.bind "ipc://#{@name}-monitor.ipc"
  end

  def setup_state_backend
    @state_backend = @context.socket ZMQ::PUB
    @state_backend.bind "ipc://#{@name}-state.ipc"
  end

  def setup_state_frontend
    @state_frontend = @context.socket ZMQ::SUB

    @peers.each do |peer|
      puts "I: connecting to state backend at #{peer}"
      @state_frontend.connect "ipc://#{peer}-state.ipc"
      @state_frontend.setsockopt ZMQ::SUBSCRIBE, peer
    end
  end
end

begin
  broker = Broker.new(ARGV.shift, ARGV)

  NUMBER_OF_WORKERS.times do
    Thread.new { Worker.new(broker.name).run }
  end

  NUMBER_OF_CIENTS.times do
    Thread.new { Client.new(broker.name).run }
  end

  broker.run
rescue ArgumentError
  puts "usage: ruby peering3.rb broker_name [peer_name ...]"
end
