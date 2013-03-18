# Broker peering simulation (part 1)
# Prototypes the state flow
#
# Translated from C by Devin Christensen: http://github.com/devin-c

require "rubygems"
require "ffi-rzmq"

class Broker
  def initialize(name, peers)
    raise ArgumentError, "A broker require's a name" unless name
    raise ArgumentError, "A broker require's peers" unless peers.any?

    @name = name
    @peers = peers
    @context = ZMQ::Context.new

    setup_state_backend
    setup_state_frontend
  end

  def run
    poller = ZMQ::Poller.new
    poller.register_readable @state_frontend

    until poller.poll(1000) == -1 do
      if poller.readables.any?
        @state_frontend.recv_string peer_name = ""
        @state_frontend.recv_string available = ""

        puts "#{peer_name} - #{available} workers free"
      else
        @state_backend.send_strings [@name, rand(10).to_s]
      end
    end

    @state_frontend.close
    @state_backend.close
    @context.terminate
  end

  private
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

  broker.run
rescue ArgumentError
  puts "usage: ruby peering1.rb broker_name [peer_name ...]"
end

