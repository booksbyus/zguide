--
--  Broker peering simulation (part 2)
--  Prototypes the request-reply flow
--
--  While this example runs in a single process, that is just to make
--  it easier to start and stop the example. Each thread has its own
--  context and conceptually acts as a separate process.
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--

require"zmq"
require"zmq.poller"
require"zmq.threads"
require"zmsg"

local tremove = table.remove

local NBR_CLIENTS  = 10
local NBR_WORKERS  = 3

local pre_code = [[
    local self, seed = ...
    local zmq = require"zmq"
    local zmsg = require"zmsg"
    require"zhelpers"
    math.randomseed(seed)
    local context = zmq.init(1)

]]

--  Request-reply client using REQ socket
--
local client_task = pre_code .. [[
    local client = context:socket(zmq.REQ)
    local endpoint = string.format("ipc://%s-localfe.ipc", self)
    assert(client:connect(endpoint))

    while true do
        --  Send request, get reply
        local msg = zmsg.new ("HELLO")
        msg:send(client)
        msg = zmsg.recv (client)
        printf ("I: client status: %s\n", msg:body())
    end
    --  We never get here but if we did, this is how we'd exit cleanly
    client:close()
    context:term()
]]

--  Worker using REQ socket to do LRU routing
--
local worker_task = pre_code .. [[
    local worker = context:socket(zmq.REQ)
    local endpoint = string.format("ipc://%s-localbe.ipc", self)
    assert(worker:connect(endpoint))

    --  Tell broker we're ready for work
    local msg = zmsg.new ("READY")
    msg:send(worker)

    while true do
        msg = zmsg.recv (worker)
        --  Do some 'work'
        s_sleep (1000)
        msg:body_fmt("OK - %04x", randof (0x10000))
        msg:send(worker)
    end
    --  We never get here but if we did, this is how we'd exit cleanly
    worker:close()
    context:term()
]]

--  First argument is this broker's name
--  Other arguments are our peers' names
--
s_version_assert (2, 1)
if (#arg < 1) then
    printf ("syntax: peering2 me doyouend...\n")
    os.exit(-1)
end
--  Our own name; in practice this'd be configured per node
local self = arg[1]
printf ("I: preparing broker at %s...\n", self)
math.randomseed(os.time())

--  Prepare our context and sockets
local context = zmq.init(1)

--  Bind cloud frontend to endpoint
local cloudfe = context:socket(zmq.ROUTER)
local endpoint = string.format("ipc://%s-cloud.ipc", self)
cloudfe:setopt(zmq.IDENTITY, self)
assert(cloudfe:bind(endpoint))

--  Connect cloud backend to all peers
local cloudbe = context:socket(zmq.ROUTER)
cloudbe:setopt(zmq.IDENTITY, self)

local peers = {}
for n=2,#arg do
    local peer = arg[n]
    -- add peer name to peers list.
    peers[#peers + 1] = peer
    peers[peer] = true -- map peer's name to 'true' for fast lookup
    printf ("I: connecting to cloud frontend at '%s'\n", peer)
    local endpoint = string.format("ipc://%s-cloud.ipc", peer)
    assert(cloudbe:connect(endpoint))
end
--  Prepare local frontend and backend
local localfe = context:socket(zmq.ROUTER)
local endpoint = string.format("ipc://%s-localfe.ipc", self)
assert(localfe:bind(endpoint))

local localbe = context:socket(zmq.ROUTER)
local endpoint = string.format("ipc://%s-localbe.ipc", self)
assert(localbe:bind(endpoint))

--  Get user to tell us when we can start...
printf ("Press Enter when all brokers are started: ")
io.read('*l')

--  Start local workers
local workers = {}
for n=1,NBR_WORKERS do
    local seed = os.time() + math.random()
    workers[n] = zmq.threads.runstring(nil, worker_task, self, seed)
    workers[n]:start(true)
end
--  Start local clients
local clients = {}
for n=1,NBR_CLIENTS do
    local seed = os.time() + math.random()
    clients[n] = zmq.threads.runstring(nil, client_task, self, seed)
    clients[n]:start(true)
end

--  Interesting part
--  -------------------------------------------------------------
--  Request-reply flow
--  - Poll backends and process local/cloud replies
--  - While worker available, route localfe to local or cloud

--  Queue of available workers
local worker_queue = {}
local backends = zmq.poller(2)

local function send_reply(msg)
    local address = msg:address()
    -- Route reply to cloud if it's addressed to a broker
    if peers[address] then
        msg:send(cloudfe) -- reply is for a peer.
    else
        msg:send(localfe) -- reply is for a local client.
    end
end

backends:add(localbe, zmq.POLLIN, function()
    local msg = zmsg.recv(localbe)

    --  Use worker address for LRU routing
    worker_queue[#worker_queue + 1] = msg:unwrap()
    -- if reply is not "READY" then route reply back to client.
    if (msg:address() ~= "READY") then
        send_reply(msg)
    end
end)

backends:add(cloudbe, zmq.POLLIN, function()
    local msg = zmsg.recv(cloudbe)
    --  We don't use peer broker address for anything
    msg:unwrap()
    -- send reply back to client.
    send_reply(msg)
end)

local frontends = zmq.poller(2)
local localfe_ready = false
local cloudfe_ready = false

frontends:add(localfe, zmq.POLLIN, function() localfe_ready = true end)
frontends:add(cloudfe, zmq.POLLIN, function() cloudfe_ready = true end)

while true do
    local timeout = (#worker_queue > 0) and 1000000 or -1
    --  If we have no workers anyhow, wait indefinitely
    rc = backends:poll(timeout)
    assert (rc >= 0)

    --  Now route as many clients requests as we can handle
    --
    while (#worker_queue > 0) do
        rc = frontends:poll(0)
        assert (rc >= 0)
        local reroutable = false
        local msg
        --  We'll do peer brokers first, to prevent starvation
        if (cloudfe_ready) then
            cloudfe_ready = false -- reset flag
            msg = zmsg.recv (cloudfe)
            reroutable = false
        elseif (localfe_ready) then
            localfe_ready = false -- reset flag
            msg = zmsg.recv (localfe)
            reroutable = true
        else
            break;      --  No work, go back to backends
        end

        --  If reroutable, send to cloud 20% of the time
        --  Here we'd normally use cloud status information
        --
        local percent = randof (5)
        if (reroutable and #peers > 0 and percent == 0) then
            --  Route to random broker peer
            local random_peer = randof (#peers) + 1
            msg:wrap(peers[random_peer], nil)
            msg:send(cloudbe)
        else
            --  Dequeue and drop the next worker address
            local worker = tremove(worker_queue, 1)
            msg:wrap(worker, "")
            msg:send(localbe)
        end
    end
end
--  We never get here but clean up anyhow
localbe:close()
cloudbe:close()
localfe:close()
cloudfe:close()
context:term()

