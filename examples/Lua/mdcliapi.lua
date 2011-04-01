--
-- mdcliapi.lua - Majordomo Protocol Client API
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

function obj_mt:set_retries(retries)
    self.retries = retries
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
    self.client = assert(self.context:socket(zmq.REQ))
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
-- Returns the reply message or nil if there was no reply.
--
function obj_mt:send(service, request)
    -- Prefix request with protocol frames
    -- Frame 1: "MDPCxy" (six bytes, MDP/Client x.y)
    -- Frame 2: Service name (printable string)
    request:push(service)
    request:push(mdp.MDPC_CLIENT)
    if self.verbose then
        s_console("I: send request to '%s' service:", service)
        request:dump()
    end

    local retries = self.retries
    while (retries > 0) do
        local msg = request:dup()
        msg:send(self.client)
        self.got_reply = false

        while true do
            local cnt = assert(self.poller:poll(self.timeout * 1000))
            if cnt ~= 0 and self.got_reply then
                local msg = zmsg.recv(self.client)
                if self.verbose then
                    s_console("I: received reply:")
                    msg:dump()
                end
                assert(msg:parts() >= 3)

                local header = msg:pop()
                assert(header == mdp.MDPC_CLIENT)
                local reply_service = msg:pop()
                assert(reply_service == service)
                return msg
            else
                retries = retries - 1
                if (retries > 0) then
                    if self.verbose then
                        s_console("W: no reply, reconnecting...")
                    end
                    -- Reconnect
                    s_mdcli_connect_to_broker(self)
                    break -- outer loop will resend request.
                else
                    if self.verbose then
                        s_console("W: permanent error, abandoning request")
                    end
                    return nil -- Giving up
                end
            end
        end
    end
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
        retries = 3,    -- before we abandon
    }, obj_mt)

    s_mdcli_connect_to_broker(self)
    return self
end

setmetatable(_M, { __call = function(self, ...) return new(...) end })

