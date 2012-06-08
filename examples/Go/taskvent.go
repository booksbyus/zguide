//
// Task ventilator
// Binds PUSH socket to tcp://localhost:5557
// Sends batch of tasks to workers via that socket
//
package main

import (
	"fmt"
	zmq "github.com/alecthomas/gozmq"
	"math/rand"
	"time"
)

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()

	// Socket to send messages On
	sender, _ := context.NewSocket(zmq.PUSH)
	defer sender.Close()
	sender.Bind("tcp://*:5557")

	//  Socket to send start of batch message on
	sink, _ := context.NewSocket(zmq.PUSH)
	defer sink.Close()
	sink.Connect("tcp://localhost:5558")

	fmt.Print("Press Enter when the workers are ready: ")

	var line string
	fmt.Scanln(&line)

	fmt.Println("Sending tasks to workers…")

	sink.Send([]byte("0"), 0)

	// Seed the random number generator
	rand.Seed(time.Now().UnixNano())

	total_msec := 0

	for i := 0; i < 100; i++ {
		workload := rand.Intn(100)
		total_msec += workload
		msg := fmt.Sprintf("%d", workload)
		sender.Send([]byte(msg), 0)
	}

	fmt.Printf("Total expected cost: %d msec\n", total_msec)

	time.Sleep(1e9) //  Give 0MQ time to deliver: one second ==  1e9 ns

}
