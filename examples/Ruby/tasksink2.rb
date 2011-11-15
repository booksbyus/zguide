#
# Task sink - design 2
# Adds pub-sub flow to send kill signal to workers
#

require 'rubygems'
require 'ffi-rzmq'

# Prepare our context and socket
context = ZMQ::Context.new(1)
receiver = context.socket(ZMQ::PULL)
receiver.bind("tcp://*:5558")

# Socket for worker control
controller = context.socket(ZMQ::PUB)
controller.bind("tcp://*:5559")

# Wait for start of batch
receiver.recv_string('')
tstart = Time.now

# Process 100 confirmations
100.times do |task_nbr|
  receiver.recv_string('')
  $stdout << ((task_nbr % 10 == 0) ? ':' : '.')
  $stdout.flush
end

# Calculate and report duration of batch
tend = Time.now
total_msec = (tend-tstart) * 1000
puts "Total elapsed time: #{total_msec} msec"

# Send kill signal to workers
controller.send_string("KILL")
