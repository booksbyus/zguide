# Titanic service
#
# Implements server side of http:#rfc.zeromq.org/spec:9
#
# Author: Tom van Leeuwen <tom@vleeuwen.eu>
# Based on Python example by Min RK

require './mdcliapi2.rb'
require './mdwrkapi.rb'
require 'pathname'
require 'securerandom'
require 'json'
require 'thread'
require 'awesome_print'

TITANIC_DIR = Pathname(Dir.pwd).join('.titanic')

def request_filename uuid
  TITANIC_DIR.join("#{uuid}.req")
end

def reply_filename uuid
  TITANIC_DIR.join("#{uuid}.rep")
end

def titanic_request pipe
  worker = MajorDomoWorker.new('tcp://localhost:5555', 'titanic.request')

  # Ensure message directory exists
  Dir.mkdir(TITANIC_DIR) unless Dir.exist?(TITANIC_DIR)

  reply = nil

  loop do
    request = worker.recv reply

    # Generate UUID and save message to disk
    uuid = SecureRandom.uuid
    filename = request_filename uuid
    File.open(filename, 'w') { |fh| fh.write request.to_json }

    # Send UUID through to message queue
    pipe.send_string uuid

    # Now send UUID back to client
    # Done by the worker.recv at the top of the loop
    reply = ["200", uuid]
  end
end

def titanic_reply
  worker = MajorDomoWorker.new('tcp://localhost:5555', 'titanic.reply')
  reply = nil
  loop do
    request = worker.recv reply
    uuid = request.shift

    if File.exist?(reply_filename(uuid))
      reply = %w[200]
      reply.concat JSON.parse(File.read(reply_filename(uuid)))
    elsif File.exist?(request_filename(uuid))
      reply = %w[300] # pending
    else
      reply = %w[400] # unknown
    end
  end
end

def titanic_close
  worker = MajorDomoWorker.new('tcp://localhost:5555', 'titanic.close')
  reply = nil
  loop do
    request = worker.recv reply
    uuid = request.shift

    File.unlink(request_filename(uuid)) if File.exist?(request_filename(uuid))
    File.unlink(reply_filename(uuid)) if File.exist?(reply_filename(uuid))

    reply = %w[200]
  end
end

def service_success client, uuid
  # Attempt to process a single request, return True if successful
  return true unless File.exist?(request_filename(uuid))

  request = JSON.parse(File.read(request_filename(uuid)))
  service = request.shift

  # Use MMI protocol to check if service is available
  mmi_request = [service]
  client.send('mmi.service', mmi_request)
  mmi_reply = client.recv

  if mmi_reply and mmi_reply.first == "200"
    client.send service, request
    reply = client.recv
    if reply
      File.open(reply_filename(uuid), 'w') { |fh| fh.write reply.to_json }
      return true
    end
  end

  false
end

context = ZMQ::Context.new

# Create MDP client session with short timeout
client = MajorDomoClient.new("tcp://localhost:5555")
client.timeout = 1000 # 1 sec
# client.retries = 1 # only 1 retry

pipe = context.socket(ZMQ::PAIR)
pipe.setsockopt ZMQ::LINGER, 0
pipe.bind("inproc://titanic")
peer = context.socket(ZMQ::PAIR)
peer.setsockopt ZMQ::LINGER, 0
peer.connect("inproc://titanic")

Thread.start do
  begin
    titanic_request peer
  rescue Exception => e
    puts e ; puts e.backtrace.join("\n")
  end
end
Thread.start do
  begin
    titanic_reply
  rescue Exception => e
    puts e ; puts e.backtrace.join("\n")
  end
end
Thread.start do
  begin
    titanic_close
  rescue Exception => e
    puts e ; puts e.backtrace.join("\n")
  end
end

poller = ZMQ::Poller.new
poller.register(pipe, ZMQ::POLLIN)

# Ensure message directory exists
Dir.mkdir(TITANIC_DIR) unless Dir.exist?(TITANIC_DIR)

# Main dispatcher loop
queue = TITANIC_DIR.join('queue')

# Ensure queue file exists and is empty
File.open(queue, 'w') { |fh| fh.write '' }

loop do
  items = poller.poll(1000)

  if items > 0
    uuid = ""
    pipe.recv_string uuid
    File.open(queue, 'a') { |fh| fh.write "-#{uuid}\n" }
  end

  # Brute-force dispatcher
  # yeah yeah... ugly
  new = []
  lines = File.read(queue).split("\n")
  lines.each do |line|
    if line =~ /^-(.*)$/
      uuid = $1
      puts "I: processing request #{uuid}"

      if service_success client, uuid
        # mark queue entry as processed
        new << uuid
      else
        new << line
      end
    else
      new << line
    end
  end

  File.open(queue, 'w') { |fh| fh.write new.join("\n") + "\n" }
end




