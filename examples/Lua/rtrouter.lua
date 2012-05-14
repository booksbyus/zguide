--
--  Cross-connected ROUTER sockets addressing each other
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"

local context = zmq.init(1)

local worker = context:socket(zmq.ROUTER)
worker:setopt(zmq.IDENTITY, "WORKER")
worker:bind("ipc://rtrouter.ipc")

local server = context:socket(zmq.ROUTER)
server:setopt(zmq.IDENTITY, "SERVER")
server:connect("ipc://rtrouter.ipc")

--  Wait for the worker to connect so that when we send a message
--  with routing envelope, it will actually match the worker...
s_sleep (1000)

server:send("WORKER", zmq.SNDMORE)
server:send("", zmq.SNDMORE)
server:send("send to worker")
s_dump     (worker)

worker:send("SERVER", zmq.SNDMORE)
worker:send("", zmq.SNDMORE)
worker:send("send to server")
s_dump     (server)

worker:close()
server:close()
context:term()


