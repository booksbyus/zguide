<<<<<<< HEAD
// Task worker - design 2
// Adds pub-sub flow to receive and respond to a kill signal
//
// Author:  Brendan Mc.
// Requires: http://github.com/alecthomas/gozmq

=======
//
// Task Wroker
// Connects PULL socket to tcp://localhost:5557
// Collects workloads from ventilator via that socket
// Connects PUSH socket to tcp://localhost:5558
// Connects SUB socket to tcp://localhost:5559
// Sends results to sink via that socket 
// 
>>>>>>> d7c9ad9f048248a1758b37af00766eb38e3df16b
package main

import (
	"fmt"
	zmq "github.com/alecthomas/gozmq"
<<<<<<< HEAD
    "time"
    "strconv"
=======
	"strconv"
	"time"
>>>>>>> d7c9ad9f048248a1758b37af00766eb38e3df16b
)

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()
<<<<<<< HEAD
    
    // Socket to receive messages on
    receiver, _ := context.NewSocket(zmq.PULL)
    defer receiver.Close()
    receiver.Connect("tcp://localhost:5557")
    
    // Socket to send messages to
    sender, _ := context.NewSocket(zmq.PUSH)
    defer sender.Close()
    sender.Connect("tcp://localhost:5558")
    
    // Socket for control input
    controller, _ := context.NewSocket(zmq.SUB);
    defer controller.Close()
    controller.Connect("tcp://localhost:5559")
    controller.SetSockOptString(zmq.SUBSCRIBE, "")
    
    // Press messages from receiver and controller
    toPoll := zmq.PollItems{
        zmq.PollItem{Socket: receiver, zmq.Events: zmq.POLLIN},
        zmq.PollItem{Socket: controller, zmq.Events: zmq.POLLIN},
    }
    
    for {
        _, _ = zmq.Poll(toPoll, -1)
        
        switch {
            case toPoll[0].REvents&zmq.POLLIN != 0:
                // Simple progress indicator for the viewer
                message, _ := toPoll[0].Socket.Recv(0)
                
                // Do really hard work
                delay, _ := strconv.ParseInt(string(message), 10, 64)
                fmt.Println(delay)
                
                time.Sleep(time.Duration(delay) * time.Millisecond)
                
                // Send results to sink
                sender.Send([]byte(""), 0)
            case toPoll[1].REvents&zmq.POLLIN != 0:
                return
        }
    }
=======

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
	controller.Connect("tcp://localhost:5559")
	controller.SetSockOptString(zmq.SUBSCRIBE, "")

	items := zmq.PollItems{
		zmq.PollItem{Socket: receiver, zmq.Events: zmq.POLLIN},
		zmq.PollItem{Socket: controller, zmq.Events: zmq.POLLIN},
	}

	//  Process tasks forever
	for {
		zmq.Poll(items, -1)
		switch {
		case items[0].REvents&zmq.POLLIN != 0:
			msgbytes, _ := receiver.Recv(0)
			fmt.Printf("%s.", string(msgbytes))

			//  Do the work
			msec, _ := strconv.ParseInt(string(msgbytes), 10, 64)
			time.Sleep(time.Duration(msec) * 1e6)

			//  Send results to sink
			sender.Send([]byte(""), 0)
		case items[1].REvents&zmq.POLLIN != 0:
			fmt.Println("stopping")
			return
		}
	}
>>>>>>> d7c9ad9f048248a1758b37af00766eb38e3df16b
}
