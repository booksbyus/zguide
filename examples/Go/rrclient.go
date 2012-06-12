// Hello World client
// Connects REQ socket to tcp://localhost:5559
// Sends "Hello" to server, expects "World" back
//
// Author:  Brendan Mc.
// Requires: http://github.com/alecthomas/gozmq

package main

import (
    "fmt"
    zmq "github.com/alecthomas/gozmq"
)

func main() {
    context, _ := zmq.NewContext()
	defer context.Close()
    
    // Socket to talk to clients
    requester, _ := context.NewSocket(zmq.REQ)
    defer requester.Close()
    requester.Connect("tcp://localhost:5559")
    
    for i := 0;i < 10;i++{
        requester.Send([]byte("Hello"), 0)
        reply, _ := requester.Recv(0)
        fmt.Printf("Received reply %d [%s]\n", i, reply)
    }
}

