#!/usr/bin/env ruby
# vim: ft=ruby

# Binary Star client proof-of-concept implementation. This client does no
# real work; it just demonstrates the Binary Star failover model.

require 'optparse'
require 'cztop'

REQUEST_TIMEOUT = 1000 # msecs
SETTLE_DELAY = 2000 # before failing over

SERVERS = %w[tcp://localhost:5001 tcp://localhost:5002]
server_nbr = 0

puts "I: connecting to server at %s…" % SERVERS[server_nbr]
client = CZTop::Socket::REQ.new(SERVERS[server_nbr])

sequence = 0
poller = CZTop::Poller.new(client)

while true
  sequence += 1
  puts sequence
  client << "#{sequence}"

  expect_reply = true
  while expect_reply
    # We use a Lazy Pirate strategy in the client. If there's no
    # reply within our timeout, we close the socket and try again.
    # In Binary Star, it's the client vote that decides which
    # server is primary; the client must therefore try to connect
    # to each server in turn:
    if poller.simple_wait(REQUEST_TIMEOUT)
      reply = client.receive
      # We got a reply from the server, must match sequence
      if reply[0].to_i == sequence
        puts "I: server replied OK (%p)" % reply[0]
        expect_reply = false
        sleep(1) # one request per second
      else
        puts "E: bad reply from server: %p" % reply
      end
    else
      puts "W: no response from server, failing over"
      # Old socket is confused; close it and open a new one
      poller.remove_reader(client)
      client.close
      server_nbr = (server_nbr + 1) % 2
      sleep(SETTLE_DELAY/1000.0)
      puts "I: connecting to server at %s…\n" % SERVERS[server_nbr]
      client = CZTop::Socket::REQ.new(SERVERS[server_nbr])
      poller.add_reader(client)
      client << "#{sequence}"
    end
  end
end
