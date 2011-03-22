--
--  Freelance server - Model 1
--  Trivial echo service
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmsg"
require"zmq"

if (#arg < 1) then
    printf("I: syntax: %s <endpoint>\n", arg[0])
    os.exit(0)
end
local context = zmq.init(1)
s_catch_signals()

--  Implement basic echo service
local server = context:socket(zmq.REP)
server:bind(arg[1])
printf("I: echo service is ready at %s\n", arg[1])
while (not s_interrupted) do
    local msg, err = zmsg.recv(server)
    if err then
        print('recv error:', err)
        break          --  Interrupted
    end
    msg:send(server)
end
if (s_interrupted) then
    printf("W: interrupted\n")
end
server:close()
context:term()


