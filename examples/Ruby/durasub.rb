# Durable subscriber
# Justin Case <justin@playelite.com>

require 'ffi-rzmq'

context = ZMQ::Context.new

# Connect our subscriber socket
subscriber = context.socket(ZMQ::SUB)
subscriber.setsockopt(ZMQ::IDENTITY, "Hello")
subscriber.setsockopt(ZMQ::SUBSCRIBE, "")
subscriber.connect("tcp://127.0.0.1:5565")

# # Synchronize with publisher
sync = context.socket(ZMQ::PUSH)
sync.connect("tcp://127.0.0.1:5564")
sync.send_string("")

# Get updates, exit when told to do so
loop do
  subscriber.recv_string(message = '')
  puts message
  if message == "END"
    break
  end
end
