# Cynical publisher for durable subscriber
# Justin Case <justin@playelite.com>

require 'ffi-rzmq'

context = ZMQ::Context.new

# Subscriber tells us when it's ready here
sync = context.socket(ZMQ::PULL)
sync.bind("tcp://127.0.0.1:5564")

# Setup socket
publisher = context.socket(ZMQ::PUB)

# Prevent publisher overflow from slow subscribers
publisher.setsockopt(ZMQ::HWM, 1)

# Specify swap space in bytes, this covers all subscribers
publisher.setsockopt(ZMQ::SWAP, 25000000)

# Accept connections on socket
publisher.bind("tcp://127.0.0.1:5565")

# Wait for synchronization request
sync_request = sync.recv_string()

# Now broadcast exactly 10 updates with pause
10.times do |update_number|
	message = sprintf("Update %d", update_number)
	publisher.send_string(message)
	sleep(1)
end

publisher.send_string("END")

sync.close
publisher.close
context.terminate
