--
--  Simple Pirate worker
--  Connects REQ socket to tcp://*:5556
--  Implements worker part of LRU queueing
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zmsg"

math.randomseed(os.time())

local context = zmq.init(1)
local worker = context:socket(zmq.REQ)

--  Set random identity to make tracing easier
local identity = string.format("%04X-%04X", randof (0x10000), randof (0x10000))
worker:setopt(zmq.IDENTITY, identity)
worker:connect("tcp://localhost:5556")

--  Tell queue we're ready for work
printf ("I: (%s) worker ready\n", identity)
worker:send("READY")

local cycles = 0
while true do
    local msg = zmsg.recv (worker)

    --  Simulate various problems, after a few cycles
    cycles = cycles + 1
    if (cycles > 3 and randof (5) == 0) then
        printf ("I: (%s) simulating a crash\n", identity)
        break
    elseif (cycles > 3 and randof (5) == 0) then
        printf ("I: (%s) simulating CPU overload\n", identity)
        s_sleep (5000)
    end
    printf ("I: (%s) normal reply - %s\n",
            identity, msg:body())
    s_sleep (1000)              --  Do some heavy work
    msg:send(worker)
end
worker:close()
context:term()


