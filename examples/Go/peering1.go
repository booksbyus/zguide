// Broker peering simulation (part 1) in Python
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

func main() {
	if len(os.Args) < 2 {
		fmt.Println("syntax: peering1 me {you}...")
		return
	}
	myself := os.Args[1]
	fmt.Printf("I: preparing broker at %s...\n", myself)
	rand.Seed(time.Now().UnixNano())

	context, _ := zmq.NewContext()
	statebe, _ := context.NewSocket(zmq.PUB)
	defer context.Close()
	defer statebe.Close()

	// Bind state backend to endpoint
	bindAddress := fmt.Sprintf("ipc://%s-state.ipc", myself)
	statebe.Bind(bindAddress)

	// Connect statefe to all peers
	statefe, _ := context.NewSocket(zmq.SUB)
	defer statefe.Close()
	statefe.SetSubscribe("")
	for i := 2; i < len(os.Args); i++ {
		peer := os.Args[i]
		fmt.Printf("I: connecting to state backend at '%s'\n", peer)
		statefe.Connect(fmt.Sprintf("ipc://%s-state.ipc", peer))
	}

	items := zmq.PollItems{
		zmq.PollItem{Socket: statefe, Events: zmq.POLLIN},
	}

	for {
		zmq.Poll(items, time.Second)
		// Handle incomming status messages
		if items[0].REvents&zmq.POLLIN != 0 {
			msg, _ := statefe.RecvMultipart(0)
			fmt.Printf("%s - %s workers free\n", string(msg[0]), string(msg[1]))
		} else {
			// Send random values for worker availability
			statebe.SendMultipart([][]byte{[]byte(myself), []byte(fmt.Sprintf("%d", rand.Intn(10)))}, 0)
		}
	}
}
