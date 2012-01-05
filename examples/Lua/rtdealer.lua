--
--  Custom routing Router to Dealer
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

local pre_code = [[
    local zmq = require"zmq"
    require"zhelpers"
    --local threads = require"zmq.threads"
    --local context = threads.get_parent_ctx()
]]

--  We have two workers, here we copy the code, normally these would
--  run on different boxes...
--
local worker_task_a = pre_code .. [[
    local context = zmq.init(1)
    local worker = context:socket(zmq.DEALER)
    worker:setopt(zmq.IDENTITY, "A")
    worker:connect("ipc://routing.ipc")

    local total = 0
    while true do
        --  We receive one part, with the workload
        local request = worker:recv()
        local finished = (request == "END")

        if (finished) then
            printf ("A received: %d\n", total)
            break
        end
        total = total + 1
    end
    worker:close()
    context:term()
]]

local worker_task_b = pre_code .. [[
    local context = zmq.init(1)
    local worker = context:socket(zmq.DEALER)
    worker:setopt(zmq.IDENTITY, "B")
    worker:connect("ipc://routing.ipc")

    local total = 0
    while true do
        --  We receive one part, with the workload
        local request = worker:recv()
        local finished = (request == "END")

        if (finished) then
            printf ("B received: %d\n", total)
            break
        end
        total = total + 1
    end
    worker:close()
    context:term()
]]

s_version_assert (2, 1)
local context = zmq.init(1)

local client = context:socket(zmq.ROUTER)
client:bind("ipc://routing.ipc")

local task_a = zmq.threads.runstring(context, worker_task_a)
task_a:start()

local task_b = zmq.threads.runstring(context, worker_task_b)
task_b:start()

--  Wait for threads to connect, since otherwise the messages
--  we send won't be routable.
s_sleep (1000)

--  Send 10 tasks scattered to A twice as often as B
math.randomseed(os.time())
for n=1,10 do
    --  Send two message parts, first the address...
    if (randof (3) > 0) then
        client:send("A", zmq.SNDMORE)
    else
        client:send("B", zmq.SNDMORE)
    end

    --  And then the workload
    client:send("This is the workload")
end
client:send("A", zmq.SNDMORE)
client:send("END")

client:send("B", zmq.SNDMORE)
client:send("END")

client:close()
context:term()

assert(task_a:join())
assert(task_b:join())

