//
//   Task sink
//   Binds PULL socket to tcp://localhost:5558
//   Collects results from workers via that socket
//   Initial Commit in examples/Go by Aaron Raddon
//   Ported to goczmq by Michael Guldan github.com/michaelcoyote
//   Requires: http://github.com/zeromq/goczmq
//
package main

import (
	"fmt"
	goczmq "github.com/zeromq/goczmq"
	"time"
)

func main() {

	//  Socket to receive messages on
	receiver := goczmq.NewSock(goczmq.Pull)
	defer receiver.Destroy()
	receiver.Bind("tcp://*:5558")

	//  Wait for start of batch
	msgbytes, _ := receiver.RecvMessage()
	fmt.Println("Received Start Msg ", string(msgbytes[0]))

	//  Start our clock now
	start_time := time.Now().UnixNano()

	//  Process 100 confirmations
	for i := 0; i < 100; i++ {
		msgbytes, _ = receiver.RecvMessage()
		fmt.Print(".")
	}

	//  Calculate and report duration of batch
	te := time.Now().UnixNano()
	fmt.Printf("Total elapsed time: %d msec\n", (te-start_time)/1e6)

}
