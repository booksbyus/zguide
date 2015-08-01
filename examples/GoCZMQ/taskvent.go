//
//   Task ventilator
//   Binds PUSH socket to tcp://localhost:5557
//   Sends batch of tasks to workers via that socket
//   Initial Commit in examples/Go bay Aaron Raddon
//   Ported to goczmq by Michael Guldan github.com/michaelcoyote
//   Requires: http://github.com/zeromq/goczmq
//
package main

import (
	"fmt"
	goczmq "github.com/zeromq/goczmq"
	"math/rand"
	"time"
)

func main() {

	// Socket to send messages On
	sender := goczmq.NewSock(goczmq.Push)
	defer sender.Destroy()
	sender.Bind("tcp://*:5557")

	//  Socket to send start of batch message on
	sink := goczmq.NewSock(goczmq.Push)
	defer sink.Destroy()
	sink.Connect("tcp://localhost:5558")

	fmt.Print("Press Enter when the workers are ready: ")

	var line string
	fmt.Scanln(&line)

	fmt.Println("Sending tasks to workers…")

	sink.SendMessage([][]byte{[]byte("0")})

	// Seed the random number generator
	rand.Seed(time.Now().UnixNano())

	total_msec := 0

	for i := 0; i < 100; i++ {
		workload := rand.Intn(100)
		total_msec += workload
		msg := []byte(fmt.Sprintf("%d", workload))
		sender.SendMessage([][]byte{msg})
	}

	fmt.Printf("Total expected cost: %d msec\n", total_msec)

	time.Sleep(1e9) //  Give 0MQ time to deliver: one second ==  1e9 ns

}
