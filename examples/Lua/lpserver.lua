--
--  Lazy Pirate server
--  Binds REQ socket to tcp://*:5555
--  Like hwserver except:
--   - echoes request as-is
--   - randomly runs slowly, or exits to simulate a crash.
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"

math.randomseed(os.time())

local context = zmq.init(1)
local server = context:socket(zmq.REP)
server:bind("tcp://*:5555")

local cycles = 0
while true do
    local request = server:recv()
    cycles = cycles + 1

    --  Simulate various problems, after a few cycles
    if (cycles > 3 and randof (3) == 0) then
        printf("I: simulating a crash\n")
        break
    elseif (cycles > 3 and randof (3) == 0) then
        printf("I: simulating CPU overload\n")
        s_sleep(2000)
    end
    printf("I: normal request (%s)\n", request)
    s_sleep(1000)              --  Do some heavy work
    server:send(request)
end
server:close()
context:term()


