// Multithreaded Hello World server.
// Uses Goroutines.  We could also use channels (a native form of 
// inproc), but I stuck to the example.
//
// Author:  Brendan Mc.
// Requires: http://github.com/alecthomas/gozmq

package main

import (
    "fmt"
    "time"
    zmq "github.com/alecthomas/gozmq"
)

func main() {
    // Launch pool of worker threads
    for i := 0;i != 5;i = i + 1 {
        go worker()
    }
    
    // Prepare our context and sockets
    context, _ := zmq.NewContext()
	defer context.Close()
    
    // Socket to talk to clients
    clients, _ := context.NewSocket(zmq.ROUTER)
    defer clients.Close()
    clients.Bind("tcp://*:5555")
    
    // Socket to talk to workers
    workers, _ := context.NewSocket(zmq.DEALER)
    defer workers.Close()
    workers.Bind("ipc://workers.ipc")
    
    // connect work threads to client threads via a queue
    zmq.Device(zmq.QUEUE, clients, workers)
    
}

func worker() {
    context, _ := zmq.NewContext()
	defer context.Close()
    
    // Socket to talk to dispatcher
    receiver, _ := context.NewSocket(zmq.REP)
    defer receiver.Close()
    receiver.Connect("ipc://workers.ipc")
    
    for true {
       received, _ := receiver.Recv(0)
       fmt.Printf("Received request [%s]\n", received)
       
       // Do some 'work'
       time.Sleep(time.Second)
       
       // Send reply back to client
       receiver.Send([]byte("World"), 0)
    }
}
