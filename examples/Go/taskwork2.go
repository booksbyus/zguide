// Task worker - design 2
// Adds pub-sub flow to receive and respond to a kill signal
//
// Author:  Brendan Mc.
// Requires: http://github.com/alecthomas/gozmq

package main

import (
	"fmt"
	zmq "github.com/alecthomas/gozmq"
    "time"
    "strconv"
)

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()
    
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
}
