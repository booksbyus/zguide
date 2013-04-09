// Synchronized subscriber
//
// Author: Aleksandar Janicijevic
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

	subscriber, _ := context.NewSocket(zmq.SUB)
	defer subscriber.Close()
	subscriber.Connect("tcp://localhost:5561")
	subscriber.SetSubscribe("")

	//  0MQ is so fast, we need to wait a while...
	time.Sleep(time.Second)

	//  Second, synchronize with publisher
	syncclient, _ := context.NewSocket(zmq.REQ)
	defer syncclient.Close()
	syncclient.Connect("tcp://localhost:5562")

	//  - send a synchronization request
	fmt.Println("Send synchronization request")
	syncclient.Send([]byte(""), 0)
	fmt.Println("Wait for synchronization reply")

	//  - wait for synchronization reply
	syncclient.Recv(0)

	fmt.Println("Get updates")
	//  Third, get our updates and report how many we got
	update_nbr := 0
	for {
		reply, _ := subscriber.Recv(0)
		if string(reply) == "END" {
			break
		}
		update_nbr++
	}
	fmt.Printf("Received %d updates\n", update_nbr)
}
