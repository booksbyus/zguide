--
-- mdcliapi2.lua - Majordomo Protocol Client API (async version)
--
-- Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--

local setmetatable = setmetatable

local mdp = require"mdp"

local zmq = require"zmq"
local zpoller = require"zmq.poller"
local zmsg = require"zmsg"
require"zhelpers"

local s_version_assert = s_version_assert

local obj_mt = {}
obj_mt.__index = obj_mt

function obj_mt:set_timeout(timeout)
    self.timeout = timeout
end

function obj_mt:destroy()
    if self.client then self.client:close() end
    self.context:term()
end

local function s_mdcli_connect_to_broker(self)
    -- close old socket.
    if self.client then
        self.poller:remove(self.client)
        self.client:close()
    end
    self.client = assert(self.context:socket(zmq.DEALER))
    assert(self.client:setopt(zmq.LINGER, 0))
    assert(self.client:connect(self.broker))
    if self.verbose then
        s_console("I: connecting to broker at %s...", self.broker)
    end
    -- add socket to poller
    self.poller:add(self.client, zmq.POLLIN, function()
        self.got_reply = true
    end)
end

--
-- Send request to broker and get reply by hook or crook
--
function obj_mt:send(service, request)
    -- Prefix request with protocol frames
    -- Frame 0: empty (REQ emulation)
    -- Frame 1: "MDPCxy" (six bytes, MDP/Client x.y)
    -- Frame 2: Service name (printable string)
    request:push(service)
    request:push(mdp.MDPC_CLIENT)
    request:push("")
    if self.verbose then
        s_console("I: send request to '%s' service:", service)
        request:dump()
    end
    request:send(self.client)
    return 0
end

--  Returns the reply message or NULL if there was no reply. Does not
--  attempt to recover from a broker failure, this is not possible
--  without storing all unanswered requests and resending them all...
function obj_mt:recv()
    self.got_reply = false

    local cnt = assert(self.poller:poll(self.timeout * 1000))
    if cnt ~= 0 and self.got_reply then
        local msg = zmsg.recv(self.client)
        if self.verbose then
            s_console("I: received reply:")
            msg:dump()
        end
        assert(msg:parts() >= 3)

        local empty = msg:pop()
        assert(empty == "")

        local header = msg:pop()
        assert(header == mdp.MDPC_CLIENT)

        return msg
    end
    if self.verbose then
        s_console("W: permanent error, abandoning request")
    end
    return nil -- Giving up
end

module(...)

function new(broker, verbose)
    s_version_assert (2, 1);
    local self = setmetatable({
        context = zmq.init(1),
        poller = zpoller.new(1),
        broker = broker,
        verbose = verbose,
        timeout = 2500, -- msecs
    }, obj_mt)

    s_mdcli_connect_to_broker(self)
    return self
end

setmetatable(_M, { __call = function(self, ...) return new(...) end })

