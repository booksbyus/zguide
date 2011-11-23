#  Custom routing Router to Papa (ROUTER to REP)
#
# libzmq: 2.1.10
# ruby: 1.9.2p180 (2011-02-18 revision 30909) [i686-linux]
# ffi-rzmq: 0.9.0
#
# @author Pavel Mitin
# @email mitin.pavel@gmail.com

require 'rubygems'
require 'ffi-rzmq'
require File.expand_path(File.join(File.dirname(__FILE__), 'zhelpers'))

#  We will do this all in one thread to emphasize the sequence of events...
context = ZMQ::Context.new 1
client = context.socket ZMQ::ROUTER
client.bind 'ipc://routing.ipc'

worker = context.socket ZMQ::REP
worker.setsockopt ZMQ::IDENTITY, 'A'
worker.connect 'ipc://routing.ipc'

# Wait for the worker to connect so that when we send a message
# with routing envelope, it will actually match the worker...
sleep 1

# Send papa address, address stack, empty part, and request
client.send_string 'A', ZMQ::SNDMORE
client.send_string 'address 3', ZMQ::SNDMORE
client.send_string 'address 2', ZMQ::SNDMORE
client.send_string 'address 1', ZMQ::SNDMORE
client.send_string '', ZMQ::SNDMORE
client.send_string 'This is the workload'

# Worker should get just the workload
s_dump worker

# We don't play with envelopes in the worker
worker.send_string 'This is the reply'

# Now dump what we got off the ROUTER socket...
s_dump client
