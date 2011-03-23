--
--  Task sink
--  Binds PULL socket to tcp://localhost:5558
--  Collects results from workers via that socket
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"
local fmod = math.fmod

--  Prepare our context and socket
local context = zmq.init(1)
local receiver = context:socket(zmq.PULL)
receiver:bind("tcp://*:5558")

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
--  Calculate and report duration of batch
printf("Total elapsed time: %d msec\n", (s_clock () - start_time))

receiver:close()
context:term()


