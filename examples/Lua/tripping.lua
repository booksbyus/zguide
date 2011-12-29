--
--  Round-trip demonstrator
--
--  While this example runs in a single process, that is just to make
--  it easier to start and stop the example. Each thread has its own
--  context and conceptually acts as a separate process.
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zmq.threads"
require"zmsg"

local common_code = [[
    require"zmq"
    require"zmsg"
    require"zhelpers"
]]

local client_task = common_code .. [[
    local context = zmq.init(1)
    local client = context:socket(zmq.DEALER)
    client:setopt(zmq.IDENTITY, "C", 1)
    client:connect("tcp://localhost:5555")

    printf("Setting up test...\n")
    s_sleep(100)

    local requests
    local start

    printf("Synchronous round-trip test...\n")
    requests = 10000
    start = s_clock()
    for n=1,requests do
        local msg = zmsg.new("HELLO")
        msg:send(client)
        msg = zmsg.recv(client)
    end
    printf(" %d calls/second\n",
        (1000 * requests) / (s_clock() - start))

    printf("Asynchronous round-trip test...\n")
    requests = 100000
    start = s_clock()
    for n=1,requests do
        local msg = zmsg.new("HELLO")
        msg:send(client)
    end
    for n=1,requests do
        local msg = zmsg.recv(client)
    end
    printf(" %d calls/second\n",
        (1000 * requests) / (s_clock() - start))

    client:close()
    context:term()
]]

local worker_task = common_code .. [[
    local context = zmq.init(1)
    local worker = context:socket(zmq.DEALER)
    worker:setopt(zmq.IDENTITY, "W", 1)
    worker:connect("tcp://localhost:5556")

    while true do
        local msg = zmsg.recv(worker)
        msg:send(worker)
    end
    worker:close()
    context:term()
]]

local broker_task = common_code .. [[
    --  Prepare our context and sockets
    local context = zmq.init(1)
    local frontend = context:socket(zmq.ROUTER)
    local backend  = context:socket(zmq.ROUTER)
    frontend:bind("tcp://*:5555")
    backend:bind("tcp://*:5556")

    require"zmq.poller"
    local poller = zmq.poller(2)
    poller:add(frontend, zmq.POLLIN, function()
        local msg = zmsg.recv(frontend)
        --msg[1] = "W"
        msg:pop()
        msg:push("W")
        msg:send(backend)
    end)
    poller:add(backend, zmq.POLLIN, function()
        local msg = zmsg.recv(backend)
        --msg[1] = "C"
        msg:pop()
        msg:push("C")
        msg:send(frontend)
    end)
    poller:start()
    frontend:close()
    backend:close()
    context:term()
]]

s_version_assert(2, 1)

local client = zmq.threads.runstring(nil, client_task)
assert(client:start())
local worker = zmq.threads.runstring(nil, worker_task)
assert(worker:start(true))
local broker = zmq.threads.runstring(nil, broker_task)
assert(broker:start(true))

assert(client:join())

