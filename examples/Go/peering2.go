// Broker peering simulation (part 2)
// Prototypes the request-reply flow
//
// Author: amyangfei <amyangfei@gmail.com>
// Requires: http://github.com/alecthomas/gozmq

package main

import (
	"fmt"
	zmq "github.com/alecthomas/gozmq"
	"math/rand"
	"os"
	"time"
)

const NBR_WORKERS = 3
const NBR_CLIENTS = 10
const WORKER_READY = "\001"

func client_task(name string, i int) {
	context, _ := zmq.NewContext()
	client, _ := context.NewSocket(zmq.REQ)
	defer context.Close()
	defer client.Close()
	client.SetIdentity(fmt.Sprintf("Client-%s-%d", name, i))
	client.Connect(fmt.Sprintf("ipc://%s-localfe.ipc", name))

	for {
		// Send request, get reply
		client.Send([]byte("HELLO"), 0)
		reply, _ := client.Recv(0)
		fmt.Printf("Client-%d: %s\n", i, reply)
		time.Sleep(time.Second)
	}
}

func worker_task(name string, i int) {
	context, _ := zmq.NewContext()
	worker, _ := context.NewSocket(zmq.REQ)
	defer context.Close()
	defer worker.Close()
	worker.SetIdentity(fmt.Sprintf("Worker-%s-%d", name, i))
	worker.Connect(fmt.Sprintf("ipc://%s-localbe.ipc", name))

	// Tell broker we're ready for work
	worker.Send([]byte(WORKER_READY), 0)

	// Process messages as they arrive
	for {
		msg, _ := worker.RecvMultipart(0)
		fmt.Printf("Worker-%d: %s\n", i, msg)
		msg[len(msg)-1] = []byte("OK")
		worker.SendMultipart(msg, 0)
	}
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("syntax: peering2 me {you}...")
		return
	}
	myself := os.Args[1]
	fmt.Printf("I: preparing broker at %s...\n", myself)
	rand.Seed(time.Now().UnixNano())

	context, _ := zmq.NewContext()
	defer context.Close()

	// Bind cloud fronted to endpoint
	cloudfe, _ := context.NewSocket(zmq.ROUTER)
	defer cloudfe.Close()
	cloudfe.SetIdentity(myself)
	cloudfe.Bind(fmt.Sprintf("ipc://%s-cloud.ipc", myself))

	// Connect cloud backend to all peers
	cloudbe, _ := context.NewSocket(zmq.ROUTER)
	defer cloudbe.Close()
	cloudbe.SetIdentity(myself)
	for i := 2; i < len(os.Args); i++ {
		peer := os.Args[i]
		fmt.Printf("I: connecting to cloud frontend at '%s'\n", peer)
		cloudbe.Connect(fmt.Sprintf("ipc://%s-cloud.ipc", peer))
	}
	// Prepare local frontend and backend
	localfe, _ := context.NewSocket(zmq.ROUTER)
	localbe, _ := context.NewSocket(zmq.ROUTER)
	defer localfe.Close()
	defer localbe.Close()
	localfe.Bind(fmt.Sprintf("ipc://%s-localfe.ipc", myself))
	localbe.Bind(fmt.Sprintf("ipc://%s-localbe.ipc", myself))

	// Get user to tell us when we can start...
	var input string
	fmt.Printf("Press Enter when all brokers are started: \n")
	fmt.Scanln(&input)

	// Start local workers
	for i := 0; i < NBR_WORKERS; i++ {
		go worker_task(myself, i)
	}

	// Start local clients
	for i := 0; i < NBR_CLIENTS; i++ {
		go client_task(myself, i)
	}

	// Interesting part
	//  Here, we handle the request-reply flow. We're using load-balancing
	//  to poll workers at all times, and clients only when there are one
	//  or more workers available.

	// Least recently used queue of available workers
	workers := make([]string, 0)

	pollerbe := zmq.PollItems{
		zmq.PollItem{Socket: localbe, Events: zmq.POLLIN},
		zmq.PollItem{Socket: cloudbe, Events: zmq.POLLIN},
	}

	pollerfe := zmq.PollItems{
		zmq.PollItem{Socket: localfe, Events: zmq.POLLIN},
		zmq.PollItem{Socket: cloudfe, Events: zmq.POLLIN},
	}

	for {
		//  If we have no workers, wait indefinitely
		timeout := time.Second
		if len(workers) == 0 {
			timeout = -1
		}
		zmq.Poll(pollerbe, timeout)

		// Handle reply from local workder
		var msg [][]byte = nil
		var err error = nil
		if pollerbe[0].REvents&zmq.POLLIN != 0 {
			msg, err = localbe.RecvMultipart(0)
			if err != nil {
				break
			}
			address, _ := msg[0], msg[1]
			msg = msg[2:]
			workers = append(workers, string(address))

			// If it's READY, don't route the message any further
			if string(msg[len(msg)-1]) == WORKER_READY {
				msg = nil
			}
		} else if pollerbe[1].REvents&zmq.POLLIN != 0 {
			msg, err = cloudbe.RecvMultipart(0)
			if err != nil {
				break
			}
			// We don't use peer broker identity for anything
			msg = msg[2:]
		}

		if msg != nil {
			address := string(msg[0])
			for i := 2; i < len(os.Args); i++ {
				// Route reply to cloud if it's addressed to a broker
				if address == os.Args[i] {
					cloudfe.SendMultipart(msg, 0)
					msg = nil
					break
				}
			}
			// Route reply to client if we still need to
			if msg != nil {
				localfe.SendMultipart(msg, 0)
			}
		}

		for len(workers) > 0 {
			zmq.Poll(pollerfe, 0)
			reroutable := false
			// We'll do peer brokers first, to prevent starvation
			if pollerfe[1].REvents&zmq.POLLIN != 0 {
				msg, _ = cloudfe.RecvMultipart(0)
				reroutable = false
			} else if pollerfe[0].REvents&zmq.POLLIN != 0 {
				msg, _ = localfe.RecvMultipart(0)
				reroutable = true
			} else {
				break // No work, go back to backends
			}

			// If reroutable, send to cloud 20% of the time
			// Here we'd normally use cloud status information
			if reroutable && len(os.Args) > 0 && rand.Intn(5) == 0 {
				// Route to random broker peer
				randPeer := rand.Intn(len(os.Args)-2) + 2
				msg = append(msg[:0], append([][]byte{[]byte(os.Args[randPeer]), []byte("")}, msg[0:]...)...)
				cloudbe.SendMultipart(msg, 0)
			} else {
				var worker string
				worker, workers = workers[0], workers[1:]
				msg = append(msg[:0], append([][]byte{[]byte(worker), []byte("")}, msg[0:]...)...)
				localbe.SendMultipart(msg, 0)
			}
		}
	}
}
