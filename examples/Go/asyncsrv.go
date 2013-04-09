// 
//  Asynchronous client-server
//  While this example runs in a single process, that is to make
//  it easier to start and stop the example. Each task has its own
//  context and conceptually acts as a separate process.
//
//  Port of asyncsrv.c
//  Written by: Aaron Clawson
package main

import (
	"fmt"
	zmq "github.com/alecthomas/gozmq"
	"math/rand"
	//"strings"
	"strconv"
	"time"
)

var finished = make(chan int)

func randomString() string {
	source := "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	target := make([]byte, 20)
	for i := 0; i < 20; i++ {
		target[i] = source[rand.Intn(len(source))]
	}
	return string(target)
}

//  This is our client task
//  It connects to the server, and then sends a request once per second
//  It collects responses as they arrive, and it prints them out. We will
//  run several client tasks in parallel, each with a different random ID.

func client_task() {
	context, _ := zmq.NewContext()
	defer context.Close()

	//  Set random identity to make tracing easier
	identity := "Client-" + randomString()

	client, _ := context.NewSocket(zmq.DEALER)
	client.SetIdentity(identity)
	client.Connect("ipc://frontend.ipc")
	defer client.Close()

	items := zmq.PollItems{
		zmq.PollItem{Socket: client, zmq.Events: zmq.POLLIN},
	}

	reqs := 0
	for {
		//Read for a response 100 times for every message we send out
		for i := 0; i < 100; i++ {
			_, err := zmq.Poll(items, time.Millisecond*10)
			if err != nil {
				break //  Interrupted
			}

			if items[0].REvents&zmq.POLLIN != 0 {
				reply, _ := client.Recv(0)
				fmt.Println(identity, "received", string(reply))
			}
		}

		reqs += 1
		req_str := "Request #" + strconv.Itoa(reqs)

		client.Send([]byte(req_str), 0)

	}

}

//  This is our server task.
//  It uses the multithreaded server model to deal requests out to a pool
//  of workers and route replies back to clients. One worker can handle
//  one request at a time but one client can talk to multiple workers at
//  once.

func server_task() {
	context, _ := zmq.NewContext()
	defer context.Close()

	//  Frontend socket talks to clients over TCP
	frontend, _ := context.NewSocket(zmq.ROUTER)
	frontend.Bind("ipc://frontend.ipc")
	defer frontend.Close()

	//  Backend socket talks to workers over inproc
	backend, _ := context.NewSocket(zmq.DEALER)
	backend.Bind("ipc://backend.ipc")
	defer backend.Close()

	//  Launch pool of worker threads, precise number is not critical	
	for i := 0; i < 5; i++ {
		go server_worker()
	}

	//  Connect backend to frontend via a proxy
	items := zmq.PollItems{
		zmq.PollItem{Socket: frontend, zmq.Events: zmq.POLLIN},
		zmq.PollItem{Socket: backend, zmq.Events: zmq.POLLIN},
	}

	for {
		_, err := zmq.Poll(items, -1)
		if err != nil {
			fmt.Println("Server exited with error:", err)
			break
		}

		if items[0].REvents&zmq.POLLIN != 0 {

			parts, _ := frontend.RecvMultipart(0)
			backend.SendMultipart(parts, 0)

		}
		if items[1].REvents&zmq.POLLIN != 0 {

			parts, _ := backend.RecvMultipart(0)
			frontend.SendMultipart(parts, 0)
		}
	}
}

//  Each worker task works on one request at a time and sends a random number
//  of replies back, with random delays between replies:
func server_worker() {
	context, _ := zmq.NewContext()
	defer context.Close()

	//  The DEALER socket gives us the reply envelope and message
	worker, _ := context.NewSocket(zmq.DEALER)
	worker.Connect("ipc://backend.ipc")
	defer worker.Close()

	for {
		parts, _ := worker.RecvMultipart(0)

		//Reply with 0..4 responses
		replies := rand.Intn(5)
		for i := 0; i < replies; i++ {
			time.Sleep(time.Duration(rand.Intn(100)) * time.Millisecond)
			worker.SendMultipart(parts, 0)
		}

	}
}

//  The main thread simply starts several clients and a server, and then
//  waits for the server to finish.

func main() {
	rand.Seed(time.Now().UTC().UnixNano())

	go client_task()
	go client_task()
	go client_task()

	go server_task()

	time.Sleep(time.Second * 5) //  Run for 5 seconds then quit
}
