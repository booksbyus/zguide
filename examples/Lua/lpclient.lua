--
--  Lazy Pirate client
--  Use zmq_poll to do a safe request-reply
--  To run, start lpserver and then randomly kill/restart it
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--

require"zmq"
require"zmq.poller"
require"zhelpers"

local REQUEST_TIMEOUT      = 2500    --  msecs, (> 1000!)
local REQUEST_RETRIES      = 3       --  Before we abandon

--  Helper function that returns a new configured socket
--  connected to the Hello World server
--
local function s_client_socket(context)
    printf ("I: connecting to server...\n")
    local client = context:socket(zmq.REQ)
    client:connect("tcp://localhost:5555")

    --  Configure socket to not wait at close time
    client:setopt(zmq.LINGER, 0)
    return client
end
s_version_assert (2, 1)
local context = zmq.init(1)
local client = s_client_socket (context)

local sequence = 0
local retries_left = REQUEST_RETRIES
local expect_reply = true

local poller = zmq.poller(1)

local function client_cb()
    --  We got a reply from the server, must match sequence
    --local reply = assert(client:recv(zmq.NOBLOCK))
    local reply = client:recv()
    if (tonumber(reply) == sequence) then
        printf ("I: server replied OK (%s)\n", reply)
        retries_left = REQUEST_RETRIES
        expect_reply = false
    else
        printf ("E: malformed reply from server: %s\n", reply)
    end
end
poller:add(client, zmq.POLLIN, client_cb)

while (retries_left > 0) do
    sequence = sequence + 1
    --  We send a request, then we work to get a reply
    local request = string.format("%d", sequence)
    client:send(request)
    expect_reply = true

    while (expect_reply) do
        --  Poll socket for a reply, with timeout
        local cnt = assert(poller:poll(REQUEST_TIMEOUT * 1000))

        -- Check if there was no reply
        if (cnt == 0) then
            retries_left = retries_left - 1
            if (retries_left == 0) then
                printf ("E: server seems to be offline, abandoning\n")
                break
            else
                printf ("W: no response from server, retrying...\n")
                --  Old socket is confused; close it and open a new one
                poller:remove(client)
                client:close()
                client = s_client_socket (context)
                poller:add(client, zmq.POLLIN, client_cb)
                --  Send request again, on new socket
                client:send(request)
            end
        end
    end
end
client:close()
context:term()

