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

	sink, _ := context.NewSocket(zmq.ROUTER)
	defer sink.Close()
	sink.Connect("inproc://example")

	//  First allow 0MQ to set the identity
	anonymous, _ := context.NewSocket(zmq.REQ)
	anonymous.Connect("inproc://example")
	anonymous.Send("ROUTER uses a generated UUID")
	anonymous.Dump()

	//  Then set the identity ourself
	identified, _ := context.NewSocket(zmq.REQ)
	identified.SetSockOptString(zmq.IDENTITY, "Hello")
	identified.Connect("inproc://example")
	identified.Send("ROUTER socket uses REQ's socket identity")
	anonymous.Dump()

}
