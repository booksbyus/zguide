--
--  Broker peering simulation (part 1)
--  Prototypes the state flow
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zmq.poller"
require"zmsg"

--  First argument is this broker's name
--  Other arguments are our peers' names
--
if (#arg < 1) then
    printf ("syntax: peering1 me doyouend...\n")
    os.exit(-1)
end
local self = arg[1]
printf ("I: preparing broker at %s...\n", self)
math.randomseed(os.time())

--  Prepare our context and sockets
local context = zmq.init(1)

--  Bind statebe to endpoint
local statebe = context:socket(zmq.PUB)
local endpoint = string.format("ipc://%s-state.ipc", self)
assert(statebe:bind(endpoint))

--  Connect statefe to all peers
local statefe = context:socket(zmq.SUB)
statefe:setopt(zmq.SUBSCRIBE, "", 0)

for n=2,#arg do
    local peer = arg[n]
    printf ("I: connecting to state backend at '%s'\n", peer)
    local endpoint = string.format("ipc://%s-state.ipc", peer)
    assert(statefe:connect(endpoint))
end

local poller = zmq.poller(1)
--  Send out status messages to peers, and collect from peers
--  The zmq_poll timeout defines our own heartbeating
--
poller:add(statefe, zmq.POLLIN, function()
    local msg = zmsg.recv (statefe)
    printf ("%s - %s workers free\n",
        msg:address(), msg:body())
end)
while true do
    --  Poll for activity, or 1 second timeout
    local count = assert(poller:poll(1000000))

    -- if no other activity.
    if count == 0 then
        --  Send random value for worker availability
        local msg = zmsg.new()
        msg:body_fmt("%d", randof (10))
        --  We stick our own address onto the envelope
        msg:wrap(self, nil)
        msg:send(statebe)
    end
end
--  We never get here but clean up anyhow
statebe:close()
statefe:close()
context:term()

