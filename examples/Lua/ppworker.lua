--
--  Paranoid Pirate worker
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zmq.poller"
require"zmsg"

local HEARTBEAT_LIVENESS   = 3       --  3-5 is reasonable
local HEARTBEAT_INTERVAL   = 1000    --  msecs
local INTERVAL_INIT        = 1000    --  Initial reconnect
local INTERVAL_MAX        = 32000    --  After exponential backoff

--  Helper function that returns a new configured socket
--  connected to the Hello World server
--
local identity

local function s_worker_socket (context)
    local worker = context:socket(zmq.DEALER)

    --  Set random identity to make tracing easier
    identity = string.format("%04X-%04X", randof (0x10000), randof (0x10000))
    worker:setopt(zmq.IDENTITY, identity)
    worker:connect("tcp://localhost:5556")

    --  Configure socket to not wait at close time
    worker:setopt(zmq.LINGER, 0)

    --  Tell queue we're ready for work
    printf("I: (%s) worker ready\n", identity)
    worker:send("READY")

    return worker
end

s_version_assert (2, 1)
math.randomseed(os.time())

local context = zmq.init(1)
local worker = s_worker_socket (context)

--  If liveness hits zero, queue is considered disconnected
local liveness = HEARTBEAT_LIVENESS
local interval = INTERVAL_INIT

--  Send out heartbeats at regular intervals
local heartbeat_at = s_clock () + HEARTBEAT_INTERVAL

local poller = zmq.poller(1)

local is_running = true

local cycles = 0
local function worker_cb()
    --  Get message
    --  - 3-part envelope + content -> request
    --  - 1-part "HEARTBEAT" -> heartbeat
    local msg = zmsg.recv (worker)

    if (msg:parts() == 3) then
        --  Simulate various problems, after a few cycles
        cycles = cycles + 1
        if (cycles > 3 and randof (5) == 0) then
            printf ("I: (%s) simulating a crash\n", identity)
            is_running = false
            return
        elseif (cycles > 3 and randof (5) == 0) then
            printf ("I: (%s) simulating CPU overload\n",
                identity)
            s_sleep (5000)
        end
        printf ("I: (%s) normal reply - %s\n",
            identity, msg:body())
        msg:send(worker)
        liveness = HEARTBEAT_LIVENESS
        s_sleep(1000);           --  Do some heavy work
    elseif (msg:parts() == 1 and msg:body() == "HEARTBEAT") then
        liveness = HEARTBEAT_LIVENESS
    else
        printf ("E: (%s) invalid message\n", identity)
        msg:dump()
    end
    interval = INTERVAL_INIT
end
poller:add(worker, zmq.POLLIN, worker_cb)

while is_running do
    local cnt = assert(poller:poll(HEARTBEAT_INTERVAL * 1000))

    if (cnt == 0) then
        liveness = liveness - 1
        if (liveness == 0) then
            printf ("W: (%s) heartbeat failure, can't reach queue\n",
                identity)
            printf ("W: (%s) reconnecting in %d msec...\n",
                identity, interval)
            s_sleep (interval)
    
            if (interval < INTERVAL_MAX) then
                interval = interval * 2
            end
            poller:remove(worker)
            worker:close()
            worker = s_worker_socket (context)
            poller:add(worker, zmq.POLLIN, worker_cb)
            liveness = HEARTBEAT_LIVENESS
        end
    end
    --  Send heartbeat to queue if it's time
    if (s_clock () > heartbeat_at) then
        heartbeat_at = s_clock () + HEARTBEAT_INTERVAL
        printf("I: (%s) worker heartbeat\n", identity)
        worker:send("HEARTBEAT")
    end
end
worker:close()
context:term()


