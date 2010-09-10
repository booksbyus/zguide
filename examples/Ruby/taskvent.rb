#
#   Task ventilator in Ruby
#   Binds PUSH socket to tcp://localhost:5557
#   Sends batch of tasks to workers via that socket
#

require 'rubygems'
require 'ffi-rzmq'

context = ZMQ::Context.new(1)

# Socket to send messages on
sender = context.socket(ZMQ::PUSH)
sender.bind("tcp://*:5557")

puts "Press enter when the workers are ready..."
$stdin.read(1)
puts "Sending tasks to workers..."

# The first message is "0" and signals start of batch
sender.send_string('0')

# Send 100 tasks
total_msec = 0  # Total expected cost in msecs
100.times do
  workload = rand(100) + 1
  total_msec += workload
  $stdout << "#{workload}."
  sender.send_string(workload.to_s)
end

puts "Total expected cost: #{total_msec} msec"
Kernel.sleep(1)  # Give 0MQ time to deliver
