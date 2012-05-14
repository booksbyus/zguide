//
// Task Wroker
// Connects PULL socket to tcp://localhost:5557
// Collects workloads from ventilator via that socket
// Connects PUSH socket to tcp://localhost:5558
// Sends results to sink via that socket 
// 
package main

import (
	"fmt"
	zmq "github.com/alecthomas/gozmq"
	"strconv"
	"time"
)

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()

	//  Socket to receive messages on
	receiver, _ := context.NewSocket(zmq.PULL)
	defer receiver.Close()
	receiver.Connect("tcp://localhost:5557")

	//  Socket to send messages to task sink
	sender, _ := context.NewSocket(zmq.PUSH)
	defer sender.Close()
	sender.Connect("tcp://localhost:5558")

	//  Process tasks forever
	for {
		msgbytes, _ := receiver.Recv(0)
		fmt.Printf("%s.\n", string(msgbytes))

		//  Do the work
		msec, _ := strconv.ParseInt(string(msgbytes), 10, 64)
		time.Sleep(time.Duration(msec) * 1e6)

		//  Send results to sink
		sender.Send([]byte(""), 0)

	}
}
