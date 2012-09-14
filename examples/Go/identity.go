//
//  Demonstrate identities as used by the request-reply pattern.  Run this
//  program by itself.  
//

package main

import (
	zmq "github.com/alecthomas/gozmq"
)

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()

	sink, err := context.NewSocket(zmq.ROUTER)
	if err != nil { print(err) }
	defer sink.Close()
	sink.Connect("inproc://example")

	// First allow 0MQ to set the identity
	anonymous, err := context.NewSocket(zmq.REQ)
	if err != nil { print(err) }
	anonymous.Connect("inproc://example")
	go anonymous.Send([]byte("ROUTER uses a generated UUID"), 0)
	sink.Recv(0)

	//  Then set the identity ourself
	identified, err := context.NewSocket(zmq.REQ)
	if err != nil { print(err) }
	identified.SetSockOptString(zmq.IDENTITY, "Hello")
	identified.Connect("inproc://example")
	go identified.Send([]byte("ROUTER socket uses REQ's socket identity"), 0)
	sink.Recv(0)
}
