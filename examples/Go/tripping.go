//
// Round-trip demonstrator
//
// Author: amyangfei <amyangfei@gmail.com>
// Requires: http://github.com/alecthomas/gozmq

package main

import (
	"fmt"
	zmq "github.com/alecthomas/gozmq"
	"time"
)

func client_task(c chan string) {
	context, _ := zmq.NewContext()
	client, _ := context.NewSocket(zmq.DEALER)
	defer context.Close()
	defer client.Close()

	client.SetIdentity("C")
	client.Connect("tcp://localhost:5555")

	fmt.Println("Setting up test...")
	time.Sleep(time.Duration(100) * time.Millisecond)

	fmt.Println("Synchronous round-trip test...")
	start := time.Now()
	requests := 10000
	for i := 0; i < requests; i++ {
		client.Send([]byte("hello"), 0)
		client.Recv(0)
	}
	fmt.Printf("%d calls/second\n", int64(float64(requests)/time.Since(start).Seconds()))

	fmt.Println("Asynchronous round-trip test...")
	start = time.Now()
	for i := 0; i < requests; i++ {
		client.Send([]byte("hello"), 0)
	}
	for i := 0; i < requests; i++ {
		client.Recv(0)
	}
	fmt.Printf("%d calls/second\n", int64(float64(requests)/time.Since(start).Seconds()))

	c <- "done"
}

func worker_task() {
	context, _ := zmq.NewContext()
	worker, _ := context.NewSocket(zmq.DEALER)
	defer context.Close()
	defer worker.Close()

	worker.SetIdentity("W")
	worker.Connect("tcp://localhost:5556")

	for {
		msg, _ := worker.RecvMultipart(0)
		worker.SendMultipart(msg, 0)
	}
}

func broker_task() {
	context, _ := zmq.NewContext()
	frontend, _ := context.NewSocket(zmq.ROUTER)
	backend, _ := context.NewSocket(zmq.ROUTER)
	defer context.Close()
	defer frontend.Close()
	defer backend.Close()
	frontend.Bind("tcp://*:5555")
	backend.Bind("tcp://*:5556")

	// Initialize poll set
	items := zmq.PollItems{
		zmq.PollItem{Socket: frontend, Events: zmq.POLLIN},
		zmq.PollItem{Socket: backend, Events: zmq.POLLIN},
	}

	for {
		zmq.Poll(items, -1)
		switch {
		case items[0].REvents&zmq.POLLIN != 0:
			msg, _ := frontend.RecvMultipart(0)
			msg[0][0] = 'W'
			backend.SendMultipart(msg, 0)
		case items[1].REvents&zmq.POLLIN != 0:
			msg, _ := backend.RecvMultipart(0)
			msg[0][0] = 'C'
			frontend.SendMultipart(msg, 0)
		}
	}
}

func main() {
	done := make(chan string)
	go client_task(done)
	go worker_task()
	go broker_task()

	<-done
}
