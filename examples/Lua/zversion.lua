--
--  Report 0MQ version
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"

print("Current 0MQ version is " .. table.concat(zmq.version(), '.'))

