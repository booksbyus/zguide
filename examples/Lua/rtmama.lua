--
--  Custom routing Router to Mama (ROUTER to REQ)
--
--  While this example runs in a single process, that is just to make
--  it easier to start and stop the example. Each thread has its own
--  context and conceptually acts as a separate process.
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zmq.threads"
require"zhelpers"

NBR_WORKERS  = 10

local pre_code = [[
    local identity, seed = ...
    local zmq = require"zmq"
    require"zhelpers"
    math.randomseed(seed)
]]

local worker_task = pre_code .. [[
    local context = zmq.init(1)
    local worker = context:socket(zmq.REQ)

    --  We use a string identity for ease here
    worker:setopt(zmq.IDENTITY, identity)
    worker:connect("ipc://routing.ipc")

    local total = 0
    while true do
        --  Tell the router we're ready for work
        worker:send("ready")

        --  Get workload from router, until finished
        local workload = worker:recv()
        local finished = (workload == "END")

        if (finished) then
            printf ("Processed: %d tasks\n", total)
            break
        end
        total = total + 1

        --  Do some random work
        s_sleep (randof (1000) + 1)
    end
    worker:close()
    context:term()
]]

s_version_assert (2, 1)
local context = zmq.init(1)

local client = context:socket(zmq.ROUTER)
client:bind("ipc://routing.ipc")
math.randomseed(os.time())

local workers = {}
for n=1,NBR_WORKERS do
    local identity = string.format("%04X-%04X", randof (0x10000), randof (0x10000))
    local seed = os.time() + math.random()
    workers[n] = zmq.threads.runstring(context, worker_task, identity, seed)
    workers[n]:start()
end
for n=1,(NBR_WORKERS * 10) do
    --  LRU worker is next waiting in queue
    local address = client:recv()
    local empty = client:recv()

    local ready = client:recv()

    client:send(address, zmq.SNDMORE)
    client:send("", zmq.SNDMORE)
    client:send("This is the workload")

end
--  Now ask mamas to shut down and report their results
for n=1,NBR_WORKERS do
    local address = client:recv()
    local empty = client:recv()

    local ready = client:recv()

    client:send(address, zmq.SNDMORE)
    client:send("", zmq.SNDMORE)
    client:send("END")

end

for n=1,NBR_WORKERS do
    assert(workers[n]:join())
end

client:close()
context:term()


