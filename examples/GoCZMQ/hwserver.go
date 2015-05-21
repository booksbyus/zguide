//
// Hello World Zeromq server
//
// Author: Aaron Raddon   github.com/araddon
// Ported to goczmq by Michael Guldan github.com/michaelcoyote
// Requires: http://github.com/zeromq/goczmq
//
package main

import (
	goczmq "github.com/zeromq/goczmq"
	"time"
)

func main() {
	socket := goczmq.NewSock(goczmq.Rep)
	defer socket.Destroy()
	socket.Bind("tcp://*:5555")

	// Wait for messages
	for {
		msg, _ := socket.RecvMessage()
		println("Received ", string(msg[0]))

		// do some fake "work"
		time.Sleep(time.Second)

		// send reply back to client
		reply := []byte("World")
		socket.SendMessage([][]byte{reply})
	}
}
