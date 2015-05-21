//
// Hello World Zeromq Client
//
// Author: Aaron Raddon   github.com/araddon
// Ported to goczmq by Michael Guldan github.com/michaelcoyote
// Requires: http://github.com/zeromq/goczmq
//
package main

import (
	"fmt"
	goczmq "github.com/zeromq/goczmq"
)

func main() {
	socket := goczmq.NewSock(goczmq.Req)
	defer socket.Destroy()

	fmt.Printf("Connecting to hello world server...")
	socket.Connect("tcp://localhost:5555")

	for i := 0; i < 10; i++ {
		// send hello
		msg := []byte(fmt.Sprintf("Hello %d", i))
		socket.SendMessage([][]byte{msg})
		println("Sending ", string(msg))

		// Wait for reply:
		reply, _ := socket.RecvMessage()
		println("Received ", string(reply[0]))
	}
}
