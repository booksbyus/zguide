// 
//   Load balancing message broker
//   Port of lbbroker.c
//   Written by: Aleksandar Janicijevic

package main

import (
	"fmt"
	zmq "github.com/alecthomas/gozmq"
	"math/rand"
	"strings"
)

const (
	NBR_CLIENTS int = 10
	NBR_WORKERS int = 3
)

func randomString() string {
	source := "abcdefghijklmnopqrstuvwxyz"
	target := make([]string, 20)
	for i := 0; i < 20; i++ {
		target[i] = string(source[rand.Intn(len(source))])
	}
	return strings.Join(target, "")
}

func set_id(socket *zmq.Socket) {
	socket.SetIdentity(randomString())
}

func client_task() {
	context, _ := zmq.NewContext()
	defer context.Close()

	client, _ := context.NewSocket(zmq.REQ)
	set_id(client)

	client.Connect("ipc://frontend.ipc")
	defer client.Close()

	//  Send request, get reply
	client.Send([]byte("HELLO"), 0)
	reply, _ := client.Recv(0)
	fmt.Println("Client: ", string(reply))
}

//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each goroutine has its own
//  context and conceptually acts as a separate process.
//  This is the worker task, using a REQ socket to do load-balancing.
//  Since s_send and s_recv can't handle 0MQ binary identities we
//  set a printable text identity to allow routing.

func worker_task() {
	context, _ := zmq.NewContext()
	defer context.Close()

	worker, _ := context.NewSocket(zmq.REQ)
	defer worker.Close()
	set_id(worker)

	worker.Connect("ipc://backend.ipc")

	//  Tell broker we're ready for work
	worker.Send([]byte("READY"), 0)

	for {
		//  Read and save all frames until we get an empty frame
		//  In this example there is only 1 but it could be more
		messageParts, _ := worker.RecvMultipart(0)
		identity := messageParts[0]
		empty := messageParts[1]
		request := messageParts[2]
		fmt.Println("Worker: ", string(request))
		worker.SendMultipart([][]byte{identity, empty, []byte("OK")}, 0)
	}
}

//  This is the main task. It starts the clients and workers, and then
//  routes requests between the two layers. Workers signal READY when
//  they start; after that we treat them as ready when they reply with
//  a response back to a client. The load-balancing data structure is
// just a queue of next available workers.
func main() {
	context, _ := zmq.NewContext()
	defer context.Close()

	frontend, _ := context.NewSocket(zmq.ROUTER)
	defer frontend.Close()
	frontend.Bind("ipc://frontend.ipc")

	backend, _ := context.NewSocket(zmq.ROUTER)
	defer backend.Close()
	backend.Bind("ipc://backend.ipc")

	var client_nbr int
	var worker_nbr int

	for client_nbr = 0; client_nbr < NBR_CLIENTS; client_nbr++ {
		go client_task()
	}

	for worker_nbr = 0; worker_nbr < NBR_WORKERS; worker_nbr++ {
		go worker_task()
	}

	//  Here is the main loop for the least-recently-used queue. It has two
	//  sockets; a frontend for clients and a backend for workers. It polls
	//  the backend in all cases, and polls the frontend only when there are
	//  one or more workers ready. This is a neat way to use 0MQ's own queues
	//  to hold messages we're not ready to process yet. When we get a client
	//  reply, we pop the next available worker, and send the request to it,
	//  including the originating client identity. When a worker replies, we
	//  re-queue that worker, and we forward the reply to the original client,
	//  using the reply envelope.

	//  Queue of available workers
	available_workers := 0
	var worker_queue []string = make([]string, 0)

	for {
		items := zmq.PollItems{
			zmq.PollItem{Socket: backend, zmq.Events: zmq.POLLIN},
			zmq.PollItem{Socket: frontend, zmq.Events: zmq.POLLIN},
		}

		//  Poll frontend only if we have available workers
		var err error
		if available_workers > 0 {
			_, err = zmq.Poll(items, -1)
		} else {
			_, err = zmq.Poll(items[:1], -1)
		}
		if err != nil {
			break //  Interrupted
		}

		//  Handle worker activity on backend
		if items[0].REvents&zmq.POLLIN != 0 {
			parts, _ := backend.RecvMultipart(0)

			//  Queue worker identity for load-balancing
			worker_id := string(parts[0])
			worker_queue = append(worker_queue, worker_id)
			available_workers++

			//  Second frame is empty
			empty := parts[1]

			//  Third frame is READY or else a client reply identity
			client_id := parts[2]

			//  If client reply, send rest back to frontend
			if string(client_id) != "READY" {
				empty = parts[3]
				reply := parts[4]
				frontend.SendMultipart([][]byte{client_id, empty, reply}, 0)
				client_nbr--
				if client_nbr == 0 {
					//  Exit after N messages
					break
				}
			}
		}

		//  Here is how we handle a client request:
		if items[1].REvents&zmq.POLLIN != 0 {
			//  Now get next client request, route to last-used worker
			//  Client request is [identity][empty][request]
			parts, _ := frontend.RecvMultipart(0)
			client_id := parts[0]
			empty := parts[1]
			request := parts[2]
			backend.SendMultipart([][]byte{[]byte(worker_queue[0]), empty, client_id,
				empty, request}, 0)

			worker_queue = worker_queue[1:]
			available_workers--
		}
	}
}
