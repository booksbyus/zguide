# Majordomo Protocol Client API, Python version.
#
# Implements the MDP/Worker spec at http:#rfc.zeromq.org/spec:7.
#
# Author: Tom van Leeuwen <tom@vleeuwen.eu>
# Based on Python example by Min RK

require 'ffi-rzmq'
require './mdp.rb'

class MajorDomoClient
  include MDP

  attr_accessor :timeout

  def initialize broker
    @broker = broker
    @context = ZMQ::Context.new(1)
    @client = nil
    @poller = ZMQ::Poller.new
    @timeout = 2500

    reconnect_to_broker
  end

  def send service, request
    request = [request] unless request.is_a?(Array)

    # Prefix request with protocol frames
    # Frame 0: empty (REQ emulation)
    # Frame 1: "MDPCxy" (six bytes, MDP/Client x.y)
    # Frame 2: Service name (printable string)
    request = ['', MDP::C_CLIENT, service].concat(request)

    @client.send_strings request
    nil
  end

  def recv
    items = @poller.poll(@timeout)
    if items
      messages = []
      @client.recv_strings messages

      messages.shift # empty

      # header
      if messages.shift != MDP::C_CLIENT
        raise RuntimeError, "Not a valid MDP message"
      end

      messages.shift # service
      return messages
    end

    nil
  end

  def reconnect_to_broker
    if @client
      @poller.deregister @client, ZMQ::DEALER
    end

    @client = @context.socket ZMQ::DEALER
    @client.setsockopt ZMQ::LINGER, 0
    @client.connect @broker
    @poller.register @client, ZMQ::POLLIN
  end
end
