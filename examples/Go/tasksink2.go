// Task design 2
// Adds pub-sub flow to send kill signal to workers
//
// Author:  Brendan Mc.
// Requires: http://github.com/alecthomas/gozmq

package main

import (
    "fmt"
    zmq "github.com/alecthomas/gozmq"
    "time"
)

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()
    
    // Socket to receive messages on
    receiver, _ := context.NewSocket(zmq.PULL)
    defer receiver.Close()
    receiver.Bind("tcp://*:5558")
    
    // Socket for worker control
    controller, _ := context.NewSocket(zmq.PUB)
    defer controller.Close()
    controller.Bind("tcp://*:5559")
    
    // Wait for start of batch
    _, _ = receiver.Recv(0)
    
    // Process 100 confirmations
    tstart := time.Now()
    for i := 0;i < 100;i = i + 1 {
        _, _ = receiver.Recv(0)
        
        if i % 10 == 0 {
            fmt.Print(":")
        } else {
            fmt.Print(".")
        }
    }
    
    total := time.Since(tstart).Nanoseconds() / 1000000
    fmt.Printf("\nTotal elapsed time:  %d msec\n", total)
    
    controller.Send([]byte("KILL"), 0)
    return
}
