// Broker peering simulation (part 3)
// Prototypes the full flow of status and tasks
//
// Author: amyangfei <amyangfei@gmail.com>
// Requires: http://github.com/alecthomas/gozmq

package main

import (
	"fmt"
	zmq "github.com/alecthomas/gozmq"
	"math/rand"
	"os"
	"strconv"
	"time"
)

const NBR_CLIENTS = 10
const NBR_WORKERS = 5
const WORKER_READY = "\001"

func client_task(name string, i int) {
	context, _ := zmq.NewContext()
	client, _ := context.NewSocket(zmq.REQ)
	monitor, _ := context.NewSocket(zmq.PUSH)
	defer context.Close()
	defer client.Close()
	defer monitor.Close()
	client.SetIdentity(fmt.Sprintf("Client-%s-%d", name, i))
	client.Connect(fmt.Sprintf("ipc://%s-localfe.ipc", name))
	monitor.Connect(fmt.Sprintf("ipc://%s-monitor.ipc", name))

	for {
		time.Sleep(time.Duration(rand.Intn(5)) * time.Second)
		burst := rand.Intn(15)
		for burst > 0 {
			burst--
			task_id := fmt.Sprintf("%04X", rand.Intn(0x10000))

			// Send request with random hex ID
			client.Send([]byte(task_id), 0)

			// Wait max ten seconds for a reply, then complain
			pollset := zmq.PollItems{
				zmq.PollItem{Socket: client, Events: zmq.POLLIN},
			}
			zmq.Poll(pollset, 10*time.Second)
			if pollset[0].REvents&zmq.POLLIN != 0 {
				reply, err := client.Recv(0)
				if err != nil {
					break
				}
				if string(reply) != task_id {
					panic("Worker is supposed to answer us with our task id")
				}
				monitor.Send(reply, 0)
			} else {
				monitor.Send([]byte(fmt.Sprintf("E: CLIENT EXIT - lost task %s", task_id)), 0)
			}
		}
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
		msg, err := worker.RecvMultipart(0)
		if err != nil {
			break
		}
		// Workers are busy for 0/1 seconds
		time.Sleep(time.Duration(rand.Intn(2)) * time.Second)
		fmt.Printf("Worker-%s-%d done: %s\n", name, i, msg)
		worker.SendMultipart(msg, 0)
	}
}

func main() {
	//  First argument is this broker's name
	//  Other arguments are our peers' names
	if len(os.Args) < 2 {
		fmt.Println("syntax: peering3 me {you}...")
		return
	}
	myself := os.Args[1]
	fmt.Printf("I: preparing broker at %s...\n", myself)
	rand.Seed(time.Now().UnixNano())

	context, _ := zmq.NewContext()
	defer context.Close()

	// Prepare local frontend and backend
	localfe, _ := context.NewSocket(zmq.ROUTER)
	localbe, _ := context.NewSocket(zmq.ROUTER)
	defer localfe.Close()
	defer localbe.Close()
	localfe.Bind(fmt.Sprintf("ipc://%s-localfe.ipc", myself))
	localbe.Bind(fmt.Sprintf("ipc://%s-localbe.ipc", myself))

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
	// Bind state backend to endpoint
	statebe, _ := context.NewSocket(zmq.PUB)
	defer statebe.Close()
	bindAddress := fmt.Sprintf("ipc://%s-state.ipc", myself)
	statebe.Bind(bindAddress)

	// Connect state frontend to all peers
	statefe, _ := context.NewSocket(zmq.SUB)
	defer statefe.Close()
	statefe.SetSubscribe("")
	for i := 2; i < len(os.Args); i++ {
		peer := os.Args[i]
		fmt.Printf("I: connecting to state backend at '%s'\n", peer)
		statefe.Connect(fmt.Sprintf("ipc://%s-state.ipc", peer))
	}
	// Prepare monitor socket
	monitor, _ := context.NewSocket(zmq.PULL)
	defer monitor.Close()
	monitor.Bind(fmt.Sprintf("ipc://%s-monitor.ipc", myself))

	// Start local workers
	for i := 0; i < NBR_WORKERS; i++ {
		go worker_task(myself, i)
	}

	// Start local clients
	for i := 0; i < NBR_CLIENTS; i++ {
		go client_task(myself, i)
	}

	// Queue of available workers
	local_capacity := 0
	cloud_capacity := 0
	workers := make([]string, 0)

	pollerbe := zmq.PollItems{
		zmq.PollItem{Socket: localbe, Events: zmq.POLLIN},
		zmq.PollItem{Socket: cloudbe, Events: zmq.POLLIN},
		zmq.PollItem{Socket: statefe, Events: zmq.POLLIN},
		zmq.PollItem{Socket: monitor, Events: zmq.POLLIN},
	}

	for {
		timeout := time.Second
		if len(workers) == 0 {
			timeout = -1
		}
		// If we have no workers anyhow, wait indefinitely
		zmq.Poll(pollerbe, timeout)

		// Track if capacity changes during this iteration
		previous := local_capacity
		var msg [][]byte = nil
		var err error = nil

		if pollerbe[0].REvents&zmq.POLLIN != 0 {
			msg, err = localbe.RecvMultipart(0)
			if err != nil {
				break
			}
			identity, _ := msg[0], msg[1]
			msg = msg[2:]
			workers = append(workers, string(identity))
			local_capacity++

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
			identity := string(msg[0])
			for i := 2; i < len(os.Args); i++ {
				// Route reply to cloud if it's addressed to a broker
				if identity == os.Args[i] {
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

		// Handle capacity updates
		if pollerbe[2].REvents&zmq.POLLIN != 0 {
			msg, _ := statefe.RecvMultipart(0)
			status := msg[1]
			cloud_capacity, _ = strconv.Atoi(string(status))
		}

		// handle monitor message
		if pollerbe[3].REvents&zmq.POLLIN != 0 {
			msg, _ := monitor.Recv(0)
			fmt.Println(string(msg))
		}

		for (local_capacity + cloud_capacity) > 0 {
			secondary := zmq.PollItems{
				zmq.PollItem{Socket: localfe, Events: zmq.POLLIN},
			}
			if local_capacity > 0 {
				secondary = append(secondary, zmq.PollItem{Socket: cloudfe, Events: zmq.POLLIN})
			}
			zmq.Poll(secondary, 0)

			if secondary[0].REvents&zmq.POLLIN != 0 {
				msg, _ = localfe.RecvMultipart(0)
			} else if len(secondary) > 1 && secondary[1].REvents&zmq.POLLIN != 0 {
				msg, _ = cloudfe.RecvMultipart(0)
			} else {
				break
			}

			if local_capacity > 0 {
				var worker string
				worker, workers = workers[0], workers[1:]
				msg = append(msg[:0], append([][]byte{[]byte(worker), []byte("")}, msg[0:]...)...)
				localbe.SendMultipart(msg, 0)
				local_capacity--
			} else {
				// Route to random broker peer
				randPeer := rand.Intn(len(os.Args)-2) + 2
				msg = append(msg[:0], append([][]byte{[]byte(os.Args[randPeer]), []byte("")}, msg[0:]...)...)
				cloudbe.SendMultipart(msg, 0)
			}
		}
		if local_capacity != previous {
			statebe.SendMultipart([][]byte{[]byte(myself), []byte(strconv.Itoa(local_capacity))}, 0)
		}
	}
}
