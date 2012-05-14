# Custom routing Router to Dealer.
# Ruby version, based on the C version from
# http://zguide.zeromq.org/chapter:all#toc45
#
# libzmq: 2.1.10
# ruby: 1.9.2p180 (2011-02-18 revision 30909) [i686-linux]
# ffi-rzmq: 0.9.0
#
# @author Pavel Mitin
# @email mitin.pavel@gmail.com

require 'rubygems'
require 'ffi-rzmq'

module RTDealer
  ENDPOINT = 'ipc://routing.ipc'
  WORKER_ADDRESSES = %w(A B)
  END_MESSAGE = 'END'

  class Worker
    def run
      do_run
    ensure
      @socket.close
    end

    private

    def initialize(context, address)
      @address = address
      @socket = context.socket ZMQ::DEALER
      @socket.setsockopt ZMQ::IDENTITY, address
      @socket.connect ENDPOINT
      @total = 0
      @workload = ''
    end

    def do_run
      catch(:end) do
        loop do
          receive_workload
          handle_workload
        end
      end
      print_results
    end

    def receive_workload
      @socket.recv_string @workload
    end

    def handle_workload
      if @workload == END_MESSAGE
        throw :end
      else
        @total += 1
      end
    end

    def print_results
      p "#{@address} received: #{@total}"
    end
  end

  class Client
    def run
      send_workload
      stop_workers
    ensure
      @socket.close
    end

    private

    def initialize(context)
      @socket = context.socket ZMQ::ROUTER
      @socket.bind ENDPOINT
    end

    def send_workload
      10.times do
        address = rand(3) % 3 == 0 ? WORKER_ADDRESSES.first : WORKER_ADDRESSES.last
        @socket.send_string address, ZMQ::SNDMORE
        @socket.send_string "This is the workload"
      end
    end

    def stop_workers
      WORKER_ADDRESSES.each do |address|
        @socket.send_string address, ZMQ::SNDMORE
        @socket.send_string END_MESSAGE
      end
    end
  end
end

if $0 == __FILE__
  context = ZMQ::Context.new 1
  client = RTDealer::Client.new context
  workers = RTDealer::WORKER_ADDRESSES.map do |address|
    Thread.new { RTDealer::Worker.new(context, address).run }
  end

  sleep 1
  client.run

  workers.each &:join
  context.terminate
end
