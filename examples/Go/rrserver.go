// Hello World server
// Connects REP socket to tcp://*:5560
// Expects "Hello" from client, replies with "World"
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
    
    // Socket to talk to clients
    responder, _ := context.NewSocket(zmq.REP)
    defer responder.Close()
    responder.Connect("tcp://localhost:5560")
    
    for {
        //  Wait for next request from client
        request, _ := responder.Recv(0)
        fmt.Printf("Received request: [%s]\n", request)
        
        // Do some 'work'
        time.Sleep(1 * time.Second)
        
        // Send reply back to client
        responder.Send([]byte("World"), 0)
    }
}
