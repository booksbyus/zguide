#
# Multithreaded relay
#

require 'rubygems'
require 'ffi-rzmq'

def step1(context)
  # Connect to step2 and tell it we're ready
  xmitter = context.socket(ZMQ::PAIR)
  xmitter.connect("inproc://step2")
  xmitter.send_string("READY")
end

def step2(context)
  # Bind inproc socket before starting step1
  receiver = context.socket(ZMQ::PAIR)
  receiver.bind("inproc://step2")
  Thread.new{step1(context)}

  # Wait for signal and pass it on
  receiver.recv_string('')

  # Connect to step3 and tell it we're ready
  xmitter = context.socket(ZMQ::PAIR)
  xmitter.connect("inproc://step3")
  xmitter.send_string("READY")
end


context = ZMQ::Context.new

# Bind inproc socket before starting step2
receiver = context.socket(ZMQ::PAIR)
receiver.bind("inproc://step3")
Thread.new{step2(context)}

# Wait for signal
receiver.recv_string('')

puts "Test successful!"
