# Majordomo Protocol Worker API, Ruby version
#
# Implements the MDP/Worker spec at http:#rfc.zeromq.org/spec:7.
#
# Author: Tom van Leeuwen <tom@vleeuwen.eu>
# Based on Python example by Min RK

require 'ffi-rzmq'
require './mdp.rb'

class MajorDomoWorker
  HEARTBEAT_LIVENESS = 3 # 3-5 is reasonable

  def initialize broker, service
    @broker = broker
    @service = service
    @context = ZMQ::Context.new(1)
    @poller = ZMQ::Poller.new
    @worker = nil # Socket to broker
    @heartbeat_at = 0 # When to send HEARTBEAT (relative to time.time(), so in seconds)
    @liveness = 0 # How many attempts left
    @timeout = 2500
    @heartbeat = 2500 # Heartbeat delay, msecs
    @reconnect = 2500 # Reconnect delay, msecs
    @expect_reply = false # false only at start
    @reply_to = nil

    reconnect_to_broker
  end

  def recv reply
    if reply and @reply_to
      reply = reply.is_a?(Array) ? [@reply_to, ''].concat(reply) : [@reply_to, '', reply]
      send_to_broker MDP::W_REPLY, nil, reply
    end

    @expect_reply = true
    loop do
      items = @poller.poll(@timeout)
      if items
        messages = []
        @worker.recv_strings messages

        @liveness = HEARTBEAT_LIVENESS

        messages.shift # empty
        if messages.shift != MDP::W_WORKER
          puts "E: Header is not MDP::WORKER"
        end

        command = messages.shift

        case command
          when MDP::W_REQUEST
            # We should pop and save as many addresses as there are
            # up to a null part, but for now, just save one...
            @reply_to = messages.shift
            messages.shift # empty
            return messages # We have a request to process
          when MDP::W_HEARTBEAT
            # do nothing
          when MDP::W_DISCONNECT
            reconnect_to_broker
          else
        end
      else
        @liveness -= 1

        if @liveness == 0
          sleep 0.001*@reconnect
          reconnect_to_broker
        end
      end

      if Time.now > @heartbeat_at
        send_to_broker MDP::W_HEARTBEAT
        @heartbeat_at = Time.now + 0.001 * @heartbeat
      end
    end
  end

  def reconnect_to_broker
    if @worker
      @poller.deregister @worker, ZMQ::DEALER
      @worker.close
    end

    @worker = @context.socket ZMQ::DEALER
    @worker.setsockopt ZMQ::LINGER, 0
    @worker.connect @broker
    @poller.register @worker, ZMQ::POLLIN
    send_to_broker(MDP::W_READY, @service, [])
    @liveness = HEARTBEAT_LIVENESS
    @heartbeat_at = Time.now + 0.001 * @heartbeat
  end

  def send_to_broker command, option=nil, message=nil
    # if no message is provided, create on internally
    if message.nil?
      message = []
    elsif not message.is_a?(Array)
      message = [message]
    end

    message = [option].concat message if option

    message = ['', MDP::W_WORKER, command].concat message
    @worker.send_strings message
  end
end