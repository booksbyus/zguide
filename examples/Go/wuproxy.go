// Weather proxy device
//
// Author:  Brendan Mc.
// Requires: http://github.com/alecthomas/gozmq

package main

import (
    zmq "github.com/alecthomas/gozmq"
)

func main() {
    context, _ := zmq.NewContext()
	defer context.Close()
    
    // This is where the weather server sits
    frontend, _ := context.NewSocket(zmq.SUB)
    defer frontend.Close()
    frontend.Connect("tcp://localhost:5556")
    
    // This is our public endpoint for subscribers
    backend, _ := context.NewSocket(zmq.PUB)
    defer backend.Close()
    backend.Bind("tcp://*:8100")
    
    // Subscribe on everything
    frontend.SetSockOptString(zmq.SUBSCRIBE, "")
    
    // Shunt messages out to our own subscribers
    for {
        message, _ := frontend.Recv(0)
        backend.Send(message, 0)
    }
}
