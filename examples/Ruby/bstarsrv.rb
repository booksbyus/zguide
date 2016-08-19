#!/usr/bin/env ruby
# vim: ft=ruby

# Binary Star server proof-of-concept implementation. This server does no
# real work; it just demonstrates the Binary Star failover model.

require 'optparse'
require 'cztop'

#  We send state information this often
#  If peer doesn't respond in two heartbeats, it is 'dead'
HEARTBEAT = 1000 # in msecs

# Binary Star finite-state machine.
class BStarState
  Exception = Class.new(StandardError)

  attr_accessor :state
  attr_writer :peer_expiry

  def initialize(state, peer_expiry = nil)
    unless [:primary, :backup].include? state
      abort "invalid initial state #{state.inspect}"
    end

    @state = state
    @peer_expiry = peer_expiry
  end

  def <<(event)
    puts "processing event #{event.inspect} ..."
    case @state
    when :primary
      case event
      when :peer_backup
        puts "I: connected to backup (passive), ready active"
        @state = :active
      when :peer_active
        puts "I: connected to backup (active), ready passive"
        @state = :passive
      end
      # Accept client connections

    when :backup
      case event
      when :peer_active
        puts "I: connected to primary (active), ready passive"
        @state = :passive
      when :client_request
        # Reject client connections when acting as backup
        raise Exception, "not active"
      end

    when :active
      case event
      when :peer_active
        # Two actives would mean split-brain
        puts "E: fatal error - dual actives, aborting"
        abort "split brain"
      end

    when :passive
      case event
      when :peer_primary
        # Peer is restarting - become active, peer will go passive
        puts "I: primary (passive) is restarting, ready active"
        @state = :active

      when :peer_backup
        # Peer is restarting - become active, peer will go passive
        puts "I: backup (passive) is restarting, ready active"
        @state = :active;

      when :peer_passive
        # Two passives would mean cluster would be non-responsive
        puts "E: fatal error - dual passives, aborting"
        abort "dual passives"

      when :client_request
        # Peer becomes active if timeout has passed
        # It's the client request that triggers the failover
        abort "bad peer expiry" unless @peer_expiry
        if Time.now >= @peer_expiry
          # If peer is dead, switch to the active state
          puts "I: failover successful, ready active"
          @state = :active
        else
          # If peer is alive, reject connections
          raise Exception, "peer is alive"
        end
      end
    end
  end
end


if __FILE__ == $0
  options = {}
  OptionParser.new do |opts|
    opts.banner = "Usage: #$0 [options]"

    opts.on("-p", "--primary", "run as primary server") do |v|
      options[:role] = :primary
    end

    opts.on("-b", "--backup", "run as backup server") do |v|
      options[:role] = :backup
    end
  end.parse!

  unless options[:role]
    abort "Usage: #$0 { -p | -b }"
  end

  # We use three sockets; one to publish state, one to subscribe to state, and
  # one for client requests/replies.
  statepub = CZTop::Socket::PUB.new
  statesub = CZTop::Socket::SUB.new
  statesub.subscribe
  frontend = CZTop::Socket::ROUTER.new

  # We bind/connect our sockets with our peer and make sure we will get state
  # messages correctly.
  case options[:role]
  when :primary
    puts "I: Primary master, waiting for backup (slave)"
    statepub.bind("tcp://*:5003")
    statesub.connect("tcp://localhost:5004")
    frontend.bind("tcp://*:5001")
    bstar = BStarState.new(:primary)
  when :backup
    puts "I: Backup slave, waiting for primary (master)"
    statepub.bind("tcp://*:5004")
    statesub.connect("tcp://localhost:5003")
    statesub.subscribe
    frontend.bind("tcp://*:5002")
    bstar = BStarState.new(:backup)
  end

  # We now process events on our two input sockets, and process these events
  # one at a time via our finite-state machine. Our "work" for a client
  # request is simply to echo it back:
  poller = CZTop::Poller.new(statesub, frontend)
  send_state_at = Time.now + (HEARTBEAT/1000.0)
  while true
    # round to msec resolution to avoid polling bursts
    time_left = (send_state_at - Time.now).round(3)
    time_left = 0 if time_left < 0
    time_left = (time_left * 1000).to_i # convert to msec

    case poller.simple_wait(time_left)
    when statesub
      # state from peer
      msg = statesub.receive
      puts "received message from statesub: #{msg.to_a.inspect}"
      bstar << :"peer_#{msg[0]}" # this could exit the process
      bstar.peer_expiry = Time.now + 2 * (HEARTBEAT/1000.0)
    when frontend
      # client request
      msg = frontend.receive
      puts "received message from frontend: #{msg.to_a.inspect}"
      begin
        bstar << :client_request
        frontend << msg
      rescue BStarState::Exception
        # We got a client request even though we're passive AND peer is alive.
        # We'll just ignore it.
      end
    end

    # If we timed out, send state to peer.
    if Time.now >= send_state_at
      statepub << bstar.state.to_s
      send_state_at = Time.now + (HEARTBEAT/1000.0)
    end
  end
end
