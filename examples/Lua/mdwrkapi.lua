--
-- mdwrkapi.lua - Majordomo Protocol Worker API
--
-- Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--

local HEARTBEAT_LIVENESS = 3  -- 3-5 is reasonable

local setmetatable = setmetatable

local mdp = require"mdp"

local zmq = require"zmq"
local zpoller = require"zmq.poller"
local zmsg = require"zmsg"
require"zhelpers"

local s_version_assert = s_version_assert

local obj_mt = {}
obj_mt.__index = obj_mt

function obj_mt:set_heartbeat(heartbeat)
    self.heartbeat = heartbeat
end

function obj_mt:set_reconnect(reconnect)
    self.reconnect = reconnect
end

function obj_mt:destroy()
    if self.worker then self.worker:close() end
    self.context:term()
end

-- Send message to broker
-- If no msg is provided, create one internally
local function s_mdwrk_send_to_broker(self, command, option, msg)
    msg = msg or zmsg.new()

    -- Stack protocol envelope to start of message
    if option then
        msg:push(option)
    end
    msg:push(command)
    msg:push(mdp.MDPW_WORKER)
    msg:push("")

    if self.verbose then
        s_console("I: sending %s to broker", mdp.mdps_commands[command])
        msg:dump()
    end
    msg:send(self.worker)
end

local function s_mdwrk_connect_to_broker(self)
    -- close old socket.
    if self.worker then
        self.poller:remove(self.worker)
        self.worker:close()
    end
    self.worker = assert(self.context:socket(zmq.DEALER))
    assert(self.worker:setopt(zmq.LINGER, 0))
    assert(self.worker:connect(self.broker))
    if self.verbose then
        s_console("I: connecting to broker at %s...", self.broker)
    end
    -- Register service with broker
    s_mdwrk_send_to_broker(self, mdp.MDPW_READY, self.service)
    -- If liveness hits zero, queue is considered disconnected
    self.liveness = HEARTBEAT_LIVENESS
    self.heartbeat_at = s_clock() + self.heartbeat
    -- add socket to poller
    self.poller:add(self.worker, zmq.POLLIN, function()
        self.got_msg = true
    end)
end

--
-- Send reply, if any, to broker and wait for next request.
--
function obj_mt:recv(reply)
    -- Format and send the reply if we are provided one
    if reply then
        assert(self.reply_to)
        reply:wrap(self.reply_to, "")
        self.reply_to = nil
        s_mdwrk_send_to_broker(self, mdp.MDPW_REPLY, nil, reply)
    end
    self.expect_reply = true

    self.got_msg = false
    while true do
        local cnt = assert(self.poller:poll(self.heartbeat * 1000))
        if cnt ~= 0 and self.got_msg then
            self.got_msg = false
            local msg = zmsg.recv(self.worker)
            if self.verbose then
                s_console("I: received message from broker:")
                msg:dump()
            end
            self.liveness = HEARTBEAT_LIVENESS
            -- Don't try to handle errors, just assert noisily
            assert(msg:parts() >= 3)

            local empty = msg:pop()
            assert(empty == "")

            local header = msg:pop()
            assert(header == mdp.MDPW_WORKER)

            local command = msg:pop()
            if command == mdp.MDPW_REQUEST then
                -- We should pop and save as many addresses as there are
                -- up to a null part, but for now, just save one...
                self.reply_to = msg:unwrap()
                return msg -- We have a request to process
            elseif command == mdp.MDPW_HEARTBEAT then
                -- Do nothing for heartbeats
            elseif command == mdp.MDPW_DISCONNECT then
                -- dis-connect and re-connect to broker.
                s_mdwrk_connect_to_broker(self)
            else
                s_console("E: invalid input message (%d)", command:byte(1,1))
                msg:dump()
            end
        else
            self.liveness = self.liveness - 1
            if (self.liveness == 0) then
                if self.verbose then
                    s_console("W: disconnected from broker - retrying...")
                end
                -- sleep then Reconnect
                s_sleep(self.reconnect)
                s_mdwrk_connect_to_broker(self)
            end

            -- Send HEARTBEAT if it's time
            if (s_clock() > self.heartbeat_at) then
                s_mdwrk_send_to_broker(self, mdp.MDPW_HEARTBEAT)
                self.heartbeat_at = s_clock() + self.heartbeat
            end
        end
    end
end

module(...)

function new(broker, service, verbose)
    s_version_assert(2, 1);
    local self = setmetatable({
        context = zmq.init(1),
        poller = zpoller.new(1),
        broker = broker,
        service = service,
        verbose = verbose,
        heartbeat = 2500, -- msecs
        reconnect = 2500, -- msecs
    }, obj_mt)

    s_mdwrk_connect_to_broker(self)
    return self
end

setmetatable(_M, { __call = function(self, ...) return new(...) end })

