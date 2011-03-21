--
--  Report 0MQ version
--
local zmq = require"zmq"

print("Current 0MQ version is " .. table.concat(zmq.version(), '.'))

