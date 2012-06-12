// Multithreaded relay.
// Uses Goroutines.  We could also use channels (a native form of 
// inproc), but I stuck to the example.
//
// Author:  Brendan Mc.
// Requires: http://github.com/alecthomas/gozmq

package main

import (
    "fmt"
    zmq "github.com/alecthomas/gozmq"
)

func main() {
    // Prepare our context and sockets
    context, _ := zmq.NewContext()
	defer context.Close()
    
    // Bind inproc socket before starting step2
    receiver, _ := context.NewSocket(zmq.PAIR)
    defer receiver.Close()
    receiver.Bind("ipc://step3.ipc")
    
    go step2()
    
    // Wait for signal
    receiver.Recv(0)
    fmt.Println("Test successful!")
}

func step1() {
    // Connect to step2 and tell it we're ready
    context, _ := zmq.NewContext()
	defer context.Close()
    
    xmitter, _ := context.NewSocket(zmq.PAIR)
    defer xmitter.Close()
    xmitter.Connect("ipc://step2.ipc")
    
    fmt.Println("Step 1 ready, signaling step 2")
    
    xmitter.Send([]byte("READY"), 0)
}

func step2() {
    context, _ := zmq.NewContext()
	defer context.Close()
    
    // Bind inproc before starting step 1
    receiver, _ := context.NewSocket(zmq.PAIR)
    defer receiver.Close()
    receiver.Bind("ipc://step2.ipc")
    
    go step1()
    
    // wait for signal and pass it on
    receiver.Recv(0)
    
    // Connect to step3 and tell it we're ready
    xmitter, _ := context.NewSocket(zmq.PAIR)
    defer xmitter.Close()
    xmitter.Connect("ipc://step3.ipc")
    
    fmt.Println("Step 2 ready, singaling step 3")
    
    xmitter.Send([]byte("READY"), 0)
}


