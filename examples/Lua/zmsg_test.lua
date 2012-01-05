--
--  Ported zmsg object tests from zmsg.h
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--

local zmsg = require"zmsg"

local tmpfile = io.tmpfile
local stderr = io.stderr

local function test_msg_save_load(msg, verbose)
    -- create temp. file.
    local file = tmpfile()
    -- print all test messages.
    if verbose then
        fprintf(stderr, "Dump test messages:\n")
        msg:dump()
    end
    -- save all messages
    msg:save(file)
    local write_len = file:seek()
    -- load all messages
    file:seek('set', 0)
    local msg2 = zmsg.load(file)
    if verbose then
        fprintf(stderr, "Load test messages from temp. file:\n")
        msg2:dump()
    end
    local read_len = file:seek()
    assert(write_len == read_len, "Different number of bytes written vs. read.")
    assert(msg == msg2, "Save/Load corrupted message.")
    file:close()
end

local msg
local part
local address
local msgs = {}

local verbose = (arg[1] == '-v')

printf(" * zmsg: ")

--  Prepare our context and sockets
local context = zmq.init(1)
local output = context:socket(zmq.DEALER)
assert(output:bind("ipc://zmsg_selftest.ipc"))
local input = context:socket(zmq.ROUTER)
assert(input:connect("ipc://zmsg_selftest.ipc"))

--  Test send and receive of single-part message
msg = zmsg.new("Hello")
assert(msg:body() == "Hello")
msgs[#msgs + 1] = msg:dup()
msg:send(output)

msg = zmsg.recv(input)
msgs[#msgs + 1] = msg:dup()
assert(msg:parts() == 2)
if verbose then
    msg:dump()
end
assert(msg:body() == "Hello")

--  Test send and receive of multi-part message
msg = zmsg.new("Hello")
msg:wrap("address1", "")
msg:wrap("address2")
assert(msg:parts() == 4)
msgs[#msgs + 1] = msg:dup()
msg:send(output)

msg = zmsg.recv(input)
msgs[#msgs + 1] = msg:dup()
if verbose then
    msg:dump()
end
assert(msg:parts() == 5)
address = msg:address()
assert(#address == 33)
address = msg:unwrap()
assert(msg:address() == "address2")
msg:body_fmt("%s%s", 'W', "orld")
msg:send(output)

msg = zmsg.recv(input)
msgs[#msgs + 1] = msg:dup()
address = msg:unwrap()
assert(msg:parts() == 4)
assert(msg:body() == "World")
local part = msg:unwrap()
assert(part == "address2")

--  Pull off address 1, check that empty part was dropped
part = msg:unwrap()
assert(part == "address1")
assert(msg:parts() == 1)

--  Check that message body was correctly modified
part = msg:pop()
assert(part == "World")
assert(msg:parts() == 0)

--  Check append method
msg:append("Hello")
msg:append("World!")
assert(msg:parts() == 2)
assert(msg:body() == "World!")

printf("OK\n")

printf(" * zmsg save/load: ")

for i=1,#msgs do
    test_msg_save_load(msgs[i], verbose)
end

printf("OK\n")

input:close()
output:close()
context:term()

