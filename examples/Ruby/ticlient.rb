# Titanic client example
# Implements client side of http:rfc.zeromq.org/spec:9
#
# Author: Tom van Leeuwen <tom@vleeuwen.eu>
# Based on Python example by Min RK

require './mdcliapi2.rb'

def service_call session, service, request
  # Returns reponse if successful (status code 200 OK), else nil
  session.send service, request
  reply = session.recv

  if reply
    status = reply.shift

    case status
      when '200'
        return reply
      when /^[45]00$/
        puts "E: client fatal error, aborting"
        exit 1
    end

    exit 0
  end
end

session = MajorDomoClient.new('tcp://localhost:5555')

#  1. Send 'echo' request to Titanic
request = ['echo', 'Hello world']
reply = service_call session, 'titanic.request', request

uuid = nil

if reply
  uuid = reply.shift
  puts "I: request UUID #{uuid}"
end

#  2. Wait until we get a reply
loop do
  sleep 10.1

  reply = service_call session, 'titanic.reply', uuid

  if reply
    puts "Reply: #{reply.last}"

    #  3. Close request
    reply = service_call session, 'titanic.close', uuid
    break
  else
    puts "I: no reply yet, trying again..."
    sleep 2
  end
end