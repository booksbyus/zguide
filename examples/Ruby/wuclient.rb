#
# Weather update client in Ruby
# Connects SUB socket to tcp://localhost:5556
# Collects weather updates and finds avg temp in zipcode
#

require 'rubygems'
require 'ffi-rzmq'

COUNT = 100

context = ZMQ::Context.new(1)

# Socket to talk to server
puts "Collecting updates from weather server..."
subscriber = context.socket(ZMQ::SUB)
subscriber.connect("tcp://localhost:5556")

# Subscribe to zipcode, default is NYC, 10001
filter = ARGV.size > 0 ? ARGV[0] : "10001 "
subscriber.setsockopt(ZMQ::SUBSCRIBE, filter)

# Process 100 updates
total_temp = 0
1.upto(COUNT) do |update_nbr|
  s = ''
  subscriber.recv_string(s)#.split.map(&:to_i)
  
  zipcode, temperature, relhumidity = s.split.map(&:to_i)
  total_temp += temperature
end

puts "Average temperature for zipcode '#{filter}' was #{total_temp / COUNT}F"
