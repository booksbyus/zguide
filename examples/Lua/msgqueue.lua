--
--  Simple message queuing broker
--  Same as request-reply broker but using QUEUE device
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"

local context = zmq.init(1)

--  Socket facing clients
local frontend = context:socket(zmq.ROUTER)
frontend:bind("tcp://*:5559")

--  Socket facing services
local backend = context:socket(zmq.DEALER)
backend:bind("tcp://*:5560")

--  Start built-in device
zmq.device(zmq.QUEUE, frontend, backend)

--  We never get here...
frontend:close()
backend:close()
context:term()

