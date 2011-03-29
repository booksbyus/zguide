--
--  Task sink - design 2
--  Adds pub-sub flow to send kill signal to workers
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"
local fmod = math.fmod

local context = zmq.init(1)

--  Socket to receive messages on
local receiver = context:socket(zmq.PULL)
receiver:bind("tcp://*:5558")

--  Socket for worker control
local controller = context:socket(zmq.PUB)
controller:bind("tcp://*:5559")

--  Wait for start of batch
local msg = receiver:recv()

--  Start our clock now
local start_time = s_clock ()

--  Process 100 confirmations
local task_nbr
for task_nbr=0,99 do
    local msg = receiver:recv()

    if (fmod(task_nbr, 10) == 0) then
        printf (":")
    else
        printf (".")
    end
    io.stdout:flush()
end
printf("Total elapsed time: %d msec\n", (s_clock () - start_time))

--  Send kill signal to workers
controller:send("KILL")

--  Finished
s_sleep (1000)              --  Give 0MQ time to deliver

receiver:close()
controller:close()
context:term()


