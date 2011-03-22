--
--  Weather update client
--  Connects SUB socket to tcp://localhost:5556
--  Collects weather updates and finds avg temp in zipcode
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"

local context = zmq.init(1)

--  Socket to talk to server
print("Collecting updates from weather server...")
local subscriber = context:socket(zmq.SUB)
subscriber:connect(arg[2] or "tcp://localhost:5556")

--  Subscribe to zipcode, default is NYC, 10001
local filter = arg[1] or "10001 "
subscriber:setopt(zmq.SUBSCRIBE, filter)

--  Process 100 updates
local update_nbr = 0
local total_temp = 0
for n=1,100 do
    local message = subscriber:recv()
    local zipcode, temperature, relhumidity = message:match("([%d-]*) ([%d-]*) ([%d-]*)")
    total_temp = total_temp + temperature
    update_nbr = update_nbr + 1
end
print(string.format("Average temperature for zipcode '%s' was %dF, total = %d",
    filter, (total_temp / update_nbr), total_temp))

subscriber:close()
context:term()
