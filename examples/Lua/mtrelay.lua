--
--  Multithreaded relay
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"
require"zmq.threads"

local pre_code = [[
    local zmq = require"zmq"
    require"zhelpers"
    local threads = require"zmq.threads"
    local context = threads.get_parent_ctx()
]]

local step1 = pre_code .. [[
    --  Connect to step2 and tell it we're ready
    local xmitter = context:socket(zmq.PAIR)
    xmitter:connect("inproc://step2")
    xmitter:send("READY")
    xmitter:close()
]]

local step2 = pre_code .. [[
    local step1 = ...
    --  Bind inproc socket before starting step1
    local receiver = context:socket(zmq.PAIR)
    receiver:bind("inproc://step2")
    local thread = zmq.threads.runstring(context, step1)
    thread:start()

    --  Wait for signal and pass it on
    local msg = receiver:recv()

    receiver:close()

    --  Connect to step3 and tell it we're ready
    local xmitter = context:socket(zmq.PAIR)
    xmitter:connect("inproc://step3")
    xmitter:send("READY")
    xmitter:close()

    assert(thread:join())
]]

s_version_assert (2, 1)
local context = zmq.init(1)

--  Bind inproc socket before starting step2
local receiver = context:socket(zmq.PAIR)
receiver:bind("inproc://step3")
local thread = zmq.threads.runstring(context, step2, step1)
thread:start()

--  Wait for signal
local msg = receiver:recv()

receiver:close()

printf ("Test successful!\n")

assert(thread:join())

context:term()


