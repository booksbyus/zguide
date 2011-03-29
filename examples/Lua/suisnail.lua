--
--  Suicidal Snail
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zmq.threads"
require"zhelpers"

--  ---------------------------------------------------------------------
--  This is our subscriber
--  It connects to the publisher and subscribes to everything. It
--  sleeps for a short time between messages to simulate doing too
--  much work. If a message is more than 1 second late, it croaks.

local subscriber = [[
    require"zmq"
    require"zhelpers"

    local MAX_ALLOWED_DELAY    = 1000    --  msecs

    local context = zmq.init(1)

    --  Subscribe to everything
    local subscriber = context:socket(zmq.SUB)
    subscriber:connect("tcp://localhost:5556")
    subscriber:setopt(zmq.SUBSCRIBE, "", 0)

    --  Get and process messages
    while true do
        local msg = subscriber:recv()
        local clock = tonumber(msg)

        --  Suicide snail logic
        if (s_clock () - clock > MAX_ALLOWED_DELAY) then
            fprintf (io.stderr, "E: subscriber cannot keep up, aborting\n")
            break
        end
        --  Work for 1 msec plus some random additional time
        s_sleep (1 + randof (2))
    end
    subscriber:close()
    context:term()
]]

--  ---------------------------------------------------------------------
--  This is our server task
--  It publishes a time-stamped message to its pub socket every 1ms.

local publisher = [[
    require"zmq"
    require"zhelpers"

    local context = zmq.init(1)

    --  Prepare publisher
    local publisher = context:socket(zmq.PUB)
    publisher:bind("tcp://*:5556")

    while true do
        --  Send current clock (msecs) to subscribers
        publisher:send(tostring(s_clock()))
        s_sleep (1);            --  1msec wait
    end
    publisher:close()
    context:term()
]]

--  This main thread simply starts a client, and a server, and then
--  waits for the client to croak.
--

local server_thread = zmq.threads.runstring(nil, publisher)
server_thread:start(true)

local client_thread = zmq.threads.runstring(nil, subscriber)
client_thread:start()
client_thread:join()


