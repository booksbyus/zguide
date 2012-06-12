# Majordomo Protocol client example. Uses the mdcliapi2 API to hide all MDP aspects
#
# Author : Tom van Leeuwen <tom@vleeuwen.eu>
# Based on Python example by Min RK

require './mdcliapi2.rb'

client = MajorDomoClient.new('tcp://localhost:5555')
requests = 100000
requests.times do |i|
  request = 'Hello world'
  begin
    client.send('echo', request)
  end
end

count = 0
while count < requests do
  begin
    reply = client.recv
  end
  count += 1
end

puts "#{count} requests/replies processed"
