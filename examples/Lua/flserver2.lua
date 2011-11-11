--
--  Freelance server - Model 2
--  Does some work, replies OK, with message sequencing
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zmsg"

if (#arg < 1) then
    printf ("I: syntax: %s <endpoint>\n", arg[0])
    os.exit (0)
end
local context = zmq.init(1)
s_catch_signals()

local server = context:socket(zmq.REP)
server:bind(arg[1])
printf ("I: service is ready at %s\n", arg[1])
while (not s_interrupted) do
    local msg, err = zmsg.recv(server)
    if err then
        print('recv error:', err)
        break          --  Interrupted
    end
    --  Fail nastily if run against wrong client
    assert (msg:parts() == 2)

    msg:body_set("OK")
    msg:send(server)
end
if (s_interrupted) then
    printf("W: interrupted\n")
end
server:close()
context:term()


