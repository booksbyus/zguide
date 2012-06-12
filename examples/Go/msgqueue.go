// Simple message queuing broker
// Same as request-reply broker but using QUEUE device
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
    
    // Socket facing clients
    frontend, _ := context.NewSocket(zmq.ROUTER)
    defer frontend.Close()
    frontend.Bind("tcp://*:5559")
    
    // Socket facing services
    backend, _ := context.NewSocket(zmq.DEALER)
    defer backend.Close()
    backend.Bind("tcp://*:5560")
    
    // Start built-in device
    zmq.Device(zmq.QUEUE, frontend, backend)
    
    // We never get here...
}
