# rtdealer example (ruby)
#
# While this example runs in a single process, that is just to make
# it easier to start and stop the example. 
#
# ffi-rzmq: 0.9.2
#
require 'ffi-rzmq'

Thread.abort_on_exception = true
context = ZMQ::Context.new(1)

def dealer_worker(context, identity)
  worker = context.socket(ZMQ::DEALER)
  worker.setsockopt(ZMQ::IDENTITY, identity)
  worker.connect("ipc://routing.ipc")

  total_work = 0
  loop do
    # We'll use socket.recvmsgs here for ease of multipart handling
    # This could also be written to use #recv_string and #more_parts?
    messages = []
    worker.recvmsgs(messages)
    # first message is a delimiter, second is the payload
    payload = messages[1].copy_out_string
    if payload == 'END'
      p "Worker #{identity} saw #{total_work} messages"
      break
    end
    total_work += 1
  end
  worker.close # work's done. clean up
end

router = context.socket(ZMQ::ROUTER)
router.bind("ipc://routing.ipc")

# Create our 'workers' each with an identity
worker_threads = []
%w{A B}.each {|id| worker_threads << Thread.new {dealer_worker(context, id)}}

# wait for threads to connect
sleep(1)

# send 10 'tasks' scattered to A twice as often as B
10.times do 
  # Here we select a worker 
  if rand(3) > 0
    router.send_string("A", ZMQ::SNDMORE)
  else
    router.send_string("B", ZMQ::SNDMORE)
  end
  # and send the payload
  router.send_string('', ZMQ::SNDMORE) # delimiter
  router.send_string("This is the payload") # payload
end

# Shutdown the workers
%w{A B}.each do |w|
  router.send_string(w, ZMQ::SNDMORE)
  router.send_string('', ZMQ::SNDMORE)
  router.send_string("END")
end
worker_threads.each {|t| t.join }

# clean up
router.close
context.terminate
