# Majordomo Protocol worker example.
#
# Author: Tom van Leeuwen <tom@vleeuwen.eu>

require './mdwrkapi.rb'

worker = MajorDomoWorker.new('tcp://localhost:5555', 'echo')
reply = nil

loop do
  request = worker.recv reply
  reply = request # Echo is complex... :-)
end


