//
//  Demonstrate identities as used by the request-reply pattern.  Run this
//  program by itself.  
//

package main

import (
	zmq "github.com/alecthomas/gozmq"
	"fmt"
)

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()

	sink, err := context.NewSocket(zmq.ROUTER)
	if err != nil { print(err) }
	defer sink.Close()
	sink.Bind("inproc://example")

	// First allow 0MQ to set the identity
	anonymous, err := context.NewSocket(zmq.REQ)
	defer anonymous.Close()
	if err != nil { fmt.Println(err) }
	anonymous.Connect("inproc://example")
	err = anonymous.Send([]byte("ROUTER uses a generated UUID"), 0)
	if err != nil { fmt.Println(err) }

	for {
	  msgdata, err := sink.Recv(0)
	  if err != nil { fmt.Println(err) }
	  fmt.Println(msgdata)
	}

	//  Then set the identity ourself
	identified, err := context.NewSocket(zmq.REQ)
	if err != nil { print(err) }
	defer identified.Close()
	identified.SetSockOptString(zmq.IDENTITY, "Hello")
	identified.Connect("inproc://example")
	identified.Send([]byte("ROUTER socket uses REQ's socket identity"), zmq.NOBLOCK)

	sink.Recv(0)
}
