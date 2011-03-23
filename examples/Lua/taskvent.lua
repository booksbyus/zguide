--
--  Task ventilator
--  Binds PUSH socket to tcp://localhost:5557
--  Sends batch of tasks to workers via that socket
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--
require"zmq"
require"zhelpers"

local context = zmq.init(1)

--  Socket to send messages on
local sender = context:socket(zmq.PUSH)
sender:bind("tcp://*:5557")

printf ("Press Enter when the workers are ready: ")
io.read('*l')
printf ("Sending tasks to workers...\n")

--  The first message is "0" and signals start of batch
sender:send("0")

--  Initialize random number generator
math.randomseed(os.time())

--  Send 100 tasks
local task_nbr
local total_msec = 0     --  Total expected cost in msecs
for task_nbr=0,99 do
    local workload
    --  Random workload from 1 to 100msecs
    workload = randof (100) + 1
    total_msec = total_msec + workload
    local msg = string.format("%d", workload)
    sender:send(msg)
end
printf ("Total expected cost: %d msec\n", total_msec)
s_sleep (1000)              --  Give 0MQ time to deliver

sender:close()
context:term()


