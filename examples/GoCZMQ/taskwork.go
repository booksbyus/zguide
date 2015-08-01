//
//   Task Worker
//   Connects PULL socket to tcp://localhost:5557
//   Collects workloads from ventilator via that socket
//   Connects PUSH socket to tcp://localhost:5558
//   Sends results to sink via that socket
//   Initial Commit in examples/Go bay Aaron Raddon
//   Ported to goczmq by Michael Guldan github.com/michaelcoyote
//   Requires: http://github.com/zeromq/goczmq
//
package main

import (
	"fmt"
	goczmq "github.com/zeromq/goczmq"
	"strconv"
	"time"
)

func main() {

	//  Socket to receive messages on
	receiver := goczmq.NewSock(goczmq.Pull)
	defer receiver.Destroy()
	receiver.Connect("tcp://localhost:5557")

	//  Socket to send messages to task sink
	sender := goczmq.NewSock(goczmq.Push)
	defer sender.Destroy()
	sender.Connect("tcp://localhost:5558")

	//  Process tasks forever
	for {
		msgbytes, _ := receiver.RecvMessage()
		fmt.Printf("%s.\n", string(msgbytes[0]))

		//  Do the work
		msec, _ := strconv.ParseInt(string(msgbytes[0]), 10, 64)
		time.Sleep(time.Duration(msec) * 1e6)

		//  Send results to sink
		sender.SendMessage([][]byte{[]byte("")})

	}
}
