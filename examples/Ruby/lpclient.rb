#!/usr/bin/ruby

# Author: Han Holl <han.holl@pobox.com>

require 'rubygems'
require 'zmq'

class LazyPirate
  def initialize(connect, retries = nil, timeout = nil)
    @connect = connect
    @retries = (retries || 3).to_i
    @timeout = (timeout || 10).to_i
    @ctx = ZMQ::Context.new(1)
    client_sock
    at_exit do
      @socket.close
    end
  end
  
  def client_sock
    @socket = @ctx.socket(ZMQ::REQ)
    @socket.setsockopt(ZMQ::LINGER, 0)
    @socket.connect(@connect)
  end

  def send(message)
    @retries.times do |tries|
      raise("Send: #{message} failed") unless @socket.send(message)
      if ZMQ.select( [@socket], nil, nil, @timeout)
        yield @socket.recv
        return
      else
        @socket.close
        client_sock
      end
    end
    raise 'Server down'
  end
      
end

if $0 == __FILE__
    server = LazyPirate.new(ARGV[0] || "tcp://localhost:599", ARGV[1], ARGV[2])
    server.send('hello there') do |response|
      puts response
    end
    puts 'success'
end

  
