--
--  Custom routing Router to Papa (ROUTER to REP)
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"

--  We will do this all in one thread to emphasize the sequence
--  of events...

local context = zmq.init(1)

local client = context:socket(zmq.ROUTER)
client:bind("ipc://routing.ipc")

local worker = context:socket(zmq.REP)
worker:setopt(zmq.IDENTITY, "A")
worker:connect("ipc://routing.ipc")

--  Wait for the worker to connect so that when we send a message
--  with routing envelope, it will actually match the worker...
s_sleep (1000)

--  Send papa address, address stack, empty part, and request
client:send("A", zmq.SNDMORE)
client:send("address 3", zmq.SNDMORE)
client:send("address 2", zmq.SNDMORE)
client:send("address 1", zmq.SNDMORE)
client:send("", zmq.SNDMORE)
client:send("This is the workload")

--  Worker should get just the workload
s_dump (worker)

--  We don't play with envelopes in the worker
worker:send("This is the reply")

--  Now dump what we got off the ROUTER socket...
s_dump (client)

client:close()
worker:close()
context:term()


