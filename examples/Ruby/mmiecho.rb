# MMI echo query example. Uses the mdcliapi2 API to hide all MDP aspects
#
# Author : Tom van Leeuwen <tom@vleeuwen.eu>

require './mdcliapi2.rb'

client = MajorDomoClient.new('tcp://localhost:5555')
client.send('mmi.service', 'echo')
reply = client.recv
puts "Lookup echo service: #{reply}"
