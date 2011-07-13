//
// Durable publisher.
//
// Author: Alec Thomas <alec@swapoff.org>
// Requires: http://github.com/alecthomas/gozmq
//
package main

import (
	"fmt"
	"time"
	zmq "github.com/alecthomas/gozmq"
)

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()

	//  Subscriber tells us when it's ready here
	sync, _ := context.NewSocket(zmq.PULL)
	defer sync.Close()
	sync.Bind("tcp://*:5564")

	//  We send updates via this socket
	publisher, _ := context.NewSocket(zmq.PUB)
	defer publisher.Close()
	publisher.Bind("tcp://*:5565")

	//  Wait for synchronization request
	sync.Recv(0)

	for update_nbr := 0; update_nbr < 10; update_nbr++ {
		str := fmt.Sprintf("Update %d", update_nbr)
		publisher.Send([]byte(str), 0)
		time.Sleep(1e9)
	}
	publisher.Send([]byte("END"), 0)

	time.Sleep(1)
}
