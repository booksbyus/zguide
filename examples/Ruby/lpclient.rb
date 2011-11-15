#!/usr/bin/ruby

# Author: Han Holl <han.holl@pobox.com>

require 'rubygems'
require 'ffi-rzmq'

class LPClient
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
    server = LPClient.new(ARGV[0] || "tcp://localhost:5555", ARGV[1], ARGV[2])
    count = 0
    loop do
      request = "#{count}"
      count += 1
      server.send(request) do |reply|
        if reply == request
          puts("I: server replied OK (#{reply})")
        else
          puts("E: malformed reply from server: #{reply}")
        end
      end
    end
    puts 'success'
end

  
