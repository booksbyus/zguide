#
# Weather update server in Ruby
# Binds PUB socket to tcp://*:5556
# Publishes random weather updates
#

require 'rubygems'
require 'ffi-rzmq'

context = ZMQ::Context.new(1)
publisher = context.socket(ZMQ::PUB)
publisher.bind("tcp://*:5556")
publisher.bind("ipc://weather.ipc")

while true
  # Get values that will fool the boss
  zipcode = rand(100000)
  temperature = rand(215) - 80
  relhumidity = rand(50) + 10

  update = "%05d %d %d" % [zipcode, temperature, relhumidity]
  puts update
  publisher.send_string(update)
end
