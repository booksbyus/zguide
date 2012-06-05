# Majordomo Protocol broker
# A minimal implementation of http:#rfc.zeromq.org/spec:7 and spec:8
#
# Author: Tom van Leeuwen <tom@vleeuwen.eu>
# Based on Python example by Min RK

require 'ffi-rzmq'
require './mdp.rb'

class MajorDomoBroker
  HEARTBEAT_INTERVAL = 2500
  HEARTBEAT_LIVENESS = 3 # 3-5 is reasonable
  HEARTBEAT_EXPIRY = HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS
  INTERNAL_SERVICE_PREFIX = 'mmi.'

  def initialize
    @context = ZMQ::Context.new
    @socket = @context.socket(ZMQ::ROUTER)
    @socket.setsockopt ZMQ::LINGER, 0
    @poller = ZMQ::Poller.new
    @poller.register @socket, ZMQ::POLLIN
    @workers = {}
    @services = {}
    @waiting = []
    @heartbeat_at = Time.now + 0.001 * HEARTBEAT_INTERVAL
  end

  def bind endpoint
    # Bind broker to endpoint, can call this multiple times.
    # We use a single socket for both clients and workers.
    @socket.bind endpoint
  end

  def mediate
    #count = 0
    loop do
      #puts "mediate: count: #{count}"
      #count += 1
      items = @poller.poll HEARTBEAT_INTERVAL
      if items > 0
        message = []
        @socket.recv_strings message
        #puts "recv: #{message.inspect}"

        address = message.shift
        message.shift # empty
        header = message.shift

        case header
          when MDP::C_CLIENT
            process_client address, message
          when MDP::W_WORKER
            process_worker address, message
          else
            puts "E: invalid messages: #{message.inspect}"
        end
      else
        #
      end

      if Time.now > @heartbeat_at
        # purge waiting expired workers
        # send heartbeats to the non expired workers
        @waiting.each do |worker|
          if Time.now > worker.expiry
            delete_worker worker
          else
            send_to_worker worker, MDP::W_HEARTBEAT
          end
        end

        puts "workers: #{@workers.count}"
        @services.each do |service, object|
          puts "service: #{service}: requests: #{object.requests.count} waiting: #{object.waiting.count}"
        end
        @heartbeat_at = Time.now + 0.001 * HEARTBEAT_INTERVAL
      end
    end
  end

  private
  def delete_worker worker, disconnect=false
    puts "delete_worker: #{worker.address.inspect} disconnect: #{disconnect}"
    send_to_worker(worker, MDP::W_DISCONNECT) if disconnect

    worker.service.waiting.delete(worker) if worker.service
    @waiting.delete worker
    @workers.delete worker.address
  end

  def send_to_worker worker, command, option=nil, message=[]
    message = [message] unless message.is_a?(Array)

    message.unshift option if option
    message.unshift command
    message.unshift MDP::W_WORKER
    message.unshift ''
    message.unshift worker.address
    #puts "send: #{message.inspect}"
    @socket.send_strings message
  end

  def process_client address, message
    service = message.shift
    message.unshift '' # empty
    message.unshift address

    if service.start_with?(INTERNAL_SERVICE_PREFIX)
      service_internal service, message
    else
      dispatch require_service(service), message
    end
  end

  def service_internal service, message
    # Handle internal service according to 8/MMI specification

    code = '501'
    if service == 'mmi.service'
      code = @services.key?(message.last) ? '200' : '404'
    end

    message.insert 2, [MDP::C_CLIENT, service]
    message[-1] = code
    message.flatten!
    @socket.send_strings message
  end

  def process_worker address, message
    command = message.shift

    worker_exists = @workers[address]
    worker = require_worker address

    case command
      when MDP::W_REPLY
        if worker_exists
          # Remove & save client return envelope and insert the
          # protocol header and service name, then rewrap envelope.
          client = message.shift
          message.shift # empty
          message = [client, '', MDP::C_CLIENT, worker.service.name].concat(message)
          @socket.send_strings message
          worker_waiting worker
        else
          delete_worker worker, true
        end
      when MDP::W_READY
        service = message.shift

        if worker_exists or service.start_with?(INTERNAL_SERVICE_PREFIX)
          delete_worker worker, true # not first command in session
        else
          worker.service = require_service service
          worker_waiting worker
        end
      when MDP::W_HEARTBEAT
        if worker_exists
          worker.expiry = Time.now + 0.001 * HEARTBEAT_EXPIRY
        else
          delete_worker worker, true
        end
      when MDP::W_DISCONNECT
        delete_worker worker
      else
        puts "E: invalid message: #{message.inspect}"
    end
  end

  def dispatch service, message
    service.requests << message if message

    while service.waiting.any? and service.requests.any?
      message = service.requests.shift
      worker = service.waiting.shift
      @waiting.delete worker
      send_to_worker worker, MDP::W_REQUEST, nil, message
    end
  end

  def require_worker address
    @workers[address] ||= Worker.new address, HEARTBEAT_EXPIRY
  end

  def require_service name
    @services[name] ||= Service.new name
  end

  def worker_waiting worker
    # This worker is waiting for work!
    @waiting << worker
    worker.service.waiting << worker
    worker.expiry = Time.now + 0.001 * HEARTBEAT_EXPIRY

    dispatch worker.service, nil
  end

  class Worker
    #attr_reader :service
    #attr_reader :identity
    attr_accessor :service
    attr_accessor :expiry
    attr_accessor :address

    #def initialize identity, address, lifetime
    def initialize address, lifetime
      #@identity = identity
      @address = address
      #@service = nil
      @expiry = Time.now + 0.001 * lifetime
    end
  end

  class Service
    attr_accessor :requests
    attr_accessor :waiting
    attr_reader :name

    def initialize name
      @name = name
      @requests = []
      @waiting = []
    end
  end
end

broker = MajorDomoBroker.new
broker.bind('tcp://*:5555')
broker.mediate

