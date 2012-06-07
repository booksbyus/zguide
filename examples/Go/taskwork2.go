//
// Task Wroker
// Connects PULL socket to tcp://localhost:5557
// Collects workloads from ventilator via that socket
// Connects PUSH socket to tcp://localhost:5558
// Connects SUB socket to tcp://localhost:5559
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

  //  Socket for control input
  controller, _ := context.NewSocket(zmq.SUB)
  defer controller.Close()
  controller.connect("tcp://localhost:5559")
  controller.SetSockOptString(zmq.SUBSCRIBE, "")

  items := make([]zmq.PollItem, 2)
  items[0].SetSocket(receiver)
  items[0].SetEvents(zmq.POLLIN)
  items[1].SetSocket(controller)
  items[1].SetEvents(zmq.POLLIN)

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
