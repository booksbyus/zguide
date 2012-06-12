// Synchronized publisher
//
// Author:  Brendan Mc.
// Requires: http://github.com/alecthomas/gozmq

package main

import (
    zmq "github.com/alecthomas/gozmq"
)

var subsExpected = 10

func main() {
    context, _ := zmq.NewContext()
	defer context.Close()
    
    // Socket to talk to clients
    publisher, _ := context.NewSocket(zmq.PUB)
    defer publisher.Close()
    publisher.Bind("tcp://*:5561")
    
    // Socket to receive signals
    syncservice, _ := context.NewSocket(zmq.REP)
    defer syncservice.Close()
    syncservice.Bind("tcp://*:5562")
    
    // Get synchronization from subscribers
    for i := 0;i < subsExpected;i = i + 1 {
        syncservice.Recv(0)
        syncservice.Send([]byte(""), 0)
    }
    
    for update_nbr := 0;update_nbr < 1000000;update_nbr = update_nbr + 1 {
        publisher.Send([]byte("Rhubarb"), 0)
    }
    
    publisher.Send([]byte("END"), 0)
}
