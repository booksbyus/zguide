--
--  Majordomo Protocol broker
--  A minimal implementation of http://rfc.zeromq.org/spec:7 and spec:8
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zmq.poller"
require"zmsg"
require"zhelpers"
require"mdp"

local tremove = table.remove

--  We'd normally pull these from config data

local HEARTBEAT_LIVENESS   = 3       --  3-5 is reasonable
local HEARTBEAT_INTERVAL   = 2500    --  msecs
local HEARTBEAT_EXPIRY     = HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS

--  ---------------------------------------------------------------------
--  Constructor for broker object

--  ---------------------------------------------------------------------
--  Broker object's metatable.
local broker_mt = {}
broker_mt.__index = broker_mt

function broker_new(verbose)
    local context = zmq.init(1)
    --  Initialize broker state
    return setmetatable({
        context = context,
        socket = context:socket(zmq.ROUTER),
        verbose = verbose,
        services = {},
        workers = {},
        waiting = {},
        heartbeat_at = s_clock() + HEARTBEAT_INTERVAL,
    }, broker_mt)
end

--  ---------------------------------------------------------------------
--  Service object
local service_mt = {}
service_mt.__index = service_mt

--  Worker object
local worker_mt = {}
worker_mt.__index = worker_mt

-- helper list remove function
local function zlist_remove(list, item)
    for n=#list,1,-1 do
        if list[n] == item then
            tremove(list, n)
        end
    end
end

--  ---------------------------------------------------------------------
--  Destructor for broker object

function broker_mt:destroy()
    self.socket:close()
    self.context:term()
    for name, service in pairs(self.services) do
        service:destroy()
    end
    for id, worker in pairs(self.workers) do
        worker:destroy()
    end
end

--  ---------------------------------------------------------------------
--  Bind broker to endpoint, can call this multiple times
--  We use a single socket for both clients and workers.

function broker_mt:bind(endpoint)
    self.socket:bind(endpoint)
    s_console("I: MDP broker/0.1.1 is active at %s", endpoint)
end

--  ---------------------------------------------------------------------
--  Delete any idle workers that haven't pinged us in a while.

function broker_mt:purge_workers()
    local waiting = self.waiting
    for n=1,#waiting do
        local worker = waiting[n]
        if (worker:expired()) then
            if (self.verbose) then
                s_console("I: deleting expired worker: %s", worker.identity)
            end

            self:worker_delete(worker, false)
        end
    end
end

--  ---------------------------------------------------------------------
--  Locate or create new service entry

function broker_mt:service_require(name)
    assert (name)
    local service = self.services[name]
    if not service then
        service = setmetatable({
            name = name,
            requests = {},
            waiting = {},
            workers = 0,
        }, service_mt)
        self.services[name] = service
        if (self.verbose) then
            s_console("I: received message:")
        end
    end
    return service
end

--  ---------------------------------------------------------------------
--  Destroy service object, called when service is removed from
--  broker.services.

function service_mt:destroy()
end

--  ---------------------------------------------------------------------
--  Dispatch requests to waiting workers as possible

function broker_mt:service_dispatch(service, msg)
    assert (service)
    local requests = service.requests
    if (msg) then               --  Queue message if any
        requests[#requests + 1] = msg
    end

    self:purge_workers()
    local waiting = service.waiting
    while (#waiting > 0 and #requests > 0) do
        local worker = tremove(waiting, 1) -- pop worker from service's waiting queue.
        zlist_remove(self.waiting, worker) -- also remove worker from broker's waiting queue.
        local msg = tremove(requests, 1) -- pop request from service's request queue.
        self:worker_send(worker, mdp.MDPW_REQUEST, nil, msg)
    end
end

--  ---------------------------------------------------------------------
--  Handle internal service according to 8/MMI specification

function broker_mt:service_internal(service_name, msg)
    if (service_name == "mmi.service") then
        local name = msg:body()
        local service = self.services[name]
        if (service and service.workers) then
            msg:body_set("200")
        else
            msg:body_set("404")
        end
    else
        msg:body_set("501")
    end

    --  Remove & save client return envelope and insert the
    --  protocol header and service name, then rewrap envelope.
    local client = msg:unwrap()
    msg:wrap(mdp.MDPC_CLIENT, service_name)
    msg:wrap(client, "")

    msg:send(self.socket)
end

--  ---------------------------------------------------------------------
--  Creates worker if necessary

function broker_mt:worker_require(identity)
    assert (identity)

    --  self.workers is keyed off worker identity
    local worker = self.workers[identity]
    if (not worker) then
        worker = setmetatable({
            identity = identity,
            expiry = 0,
        }, worker_mt)
        self.workers[identity] = worker
        if (self.verbose) then
            s_console("I: registering new worker: %s", identity)
        end
    end
    return worker
end

--  ---------------------------------------------------------------------
--  Deletes worker from all data structures, and destroys worker

function broker_mt:worker_delete(worker, disconnect)
    assert (worker)
    if (disconnect) then
        self:worker_send(worker, mdp.MDPW_DISCONNECT)
    end
    local service = worker.service
    if (service) then
        zlist_remove (service.waiting, worker)
        service.workers = service.workers - 1
    end
    zlist_remove (self.waiting, worker)
    self.workers[worker.identity] = nil
    worker:destroy()
end

--  ---------------------------------------------------------------------
--  Destroy worker object, called when worker is removed from
--  broker.workers.

function worker_mt:destroy(argument)
end

--  ---------------------------------------------------------------------
--  Process message sent to us by a worker

function broker_mt:worker_process(sender, msg)
    assert (msg:parts() >= 1)     --  At least, command

    local command = msg:pop()
    local worker_ready = (self.workers[sender] ~= nil)
    local worker = self:worker_require(sender)

    if (command == mdp.MDPW_READY) then
        if (worker_ready) then          --  Not first command in session then
            self:worker_delete(worker, true)
        elseif (sender:sub(1,4) == "mmi.") then  --  Reserved service name
            self:worker_delete(worker, true)
        else
            --  Attach worker to service and mark as idle
            local service_name = msg:pop()
            local service = self:service_require(service_name)
            worker.service = service
            service.workers = service.workers + 1
            self:worker_waiting(worker)
        end
    elseif (command == mdp.MDPW_REPLY) then
        if (worker_ready) then
            --  Remove & save client return envelope and insert the
            --  protocol header and service name, then rewrap envelope.
            local client = msg:unwrap()
            msg:wrap(mdp.MDPC_CLIENT, worker.service.name)
            msg:wrap(client, "")

            msg:send(self.socket)
            self:worker_waiting(worker)
        else
            self:worker_delete(worker, true)
        end
    elseif (command == mdp.MDPW_HEARTBEAT) then
        if (worker_ready) then
            worker.expiry = s_clock() + HEARTBEAT_EXPIRY
        else
            self:worker_delete(worker, true)
        end
    elseif (command == mdp.MDPW_DISCONNECT) then
        self:worker_delete(worker, false)
    else
        s_console("E: invalid input message (%d)", command:byte(1,1))
        msg:dump()
    end
end

--  ---------------------------------------------------------------------
--  Send message to worker
--  If pointer to message is provided, sends & destroys that message

function broker_mt:worker_send(worker, command, option, msg)
    msg = msg and msg:dup() or zmsg.new()

    --  Stack protocol envelope to start of message
    if (option) then                 --  Optional frame after command
        msg:push(option)
    end
    msg:push(command)
    msg:push(mdp.MDPW_WORKER)
    --  Stack routing envelope to start of message
    msg:wrap(worker.identity, "")

    if (self.verbose) then
        s_console("I: sending %s to worker", mdp.mdps_commands[command])
        msg:dump()
    end
    msg:send(self.socket)
end

--  ---------------------------------------------------------------------
--  This worker is now waiting for work

function broker_mt:worker_waiting(worker)
    --  Queue to broker and service waiting lists
    self.waiting[#self.waiting + 1] = worker
    worker.service.waiting[#worker.service.waiting + 1] = worker
    worker.expiry = s_clock() + HEARTBEAT_EXPIRY
    self:service_dispatch(worker.service, nil)
end

--  ---------------------------------------------------------------------
--  Return 1 if worker has expired and must be deleted

function worker_mt:expired()
    return (self.expiry < s_clock())
end
--  ---------------------------------------------------------------------
--  Process a request coming from a client

function broker_mt:client_process(sender, msg)
    assert (msg:parts() >= 2)     --  Service name + body

    local service_name = msg:pop()
    local service = self:service_require(service_name)
    --  Set reply return address to client sender
    msg:wrap(sender, "")
    if (service_name:sub(1,4) == "mmi.") then
        self:service_internal(service_name, msg)
    else
        self:service_dispatch(service, msg)
    end
end


--  ---------------------------------------------------------------------
--  Main broker work happens here

local verbose = (arg[1] == "-v")

s_version_assert (2, 1)
s_catch_signals ()
local self = broker_new(verbose)
self:bind("tcp://*:5555")

local poller = zmq.poller.new(1)

--  Process next input message, if any
poller:add(self.socket, zmq.POLLIN, function()
    local msg = zmsg.recv(self.socket)
    if (self.verbose) then
        s_console("I: received message:")
        msg:dump()
    end
    local sender = msg:pop()
    local empty  = msg:pop()
    local header = msg:pop()

    if (header == mdp.MDPC_CLIENT) then
        self:client_process(sender, msg)
    elseif (header == mdp.MDPW_WORKER) then
        self:worker_process(sender, msg)
    else
        s_console("E: invalid message:")
        msg:dump()
    end
end)

--  Get and process messages forever or until interrupted
while (not s_interrupted) do
    local cnt = assert(poller:poll(HEARTBEAT_INTERVAL * 1000))
    --  Disconnect and delete any expired workers
    --  Send heartbeats to idle workers if needed
    if (s_clock() > self.heartbeat_at) then
        self:purge_workers()
        local waiting = self.waiting
        for n=1,#waiting do
            local worker = waiting[n]
            self:worker_send(worker, mdp.MDPW_HEARTBEAT)
        end
        self.heartbeat_at = s_clock() + HEARTBEAT_INTERVAL
    end
end
if (s_interrupted) then
    printf("W: interrupt received, shutting down...\n")
end
self:destroy()


