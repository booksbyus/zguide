//  Simple Pirate worker
//  Connects REQ socket to tcp://*:5556
//  Implements worker part of load-balancing
//
//  Author: iano <scaly.iano@gmail.com>
//  Based on C & Python example

package main

import (
	"fmt"
	zmq "github.com/alecthomas/gozmq"
	"math/rand"
	"time"
)

const LRU_READY = "\001"

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()

	worker, _ := context.NewSocket(zmq.REQ)
	defer worker.Close()

	//  Set random identity to make tracing easier
	src := rand.NewSource(time.Now().UnixNano())
	random := rand.New(src)
	identity := fmt.Sprintf("%04X-%04X", random.Intn(0x10000), random.Intn(0x10000))
	worker.SetIdentity(identity)
	worker.Connect("tcp://localhost:5556")

	//  Tell broker we're ready for work
	fmt.Printf("I: (%s) worker ready\n", identity)
	worker.Send([]byte(LRU_READY), 0)

	for cycles := 1; ; cycles++ {
		msg, err := worker.RecvMultipart(0)
		if err != nil {
			panic(err) //  Interrupted
		}

		if cycles > 3 {
			switch r := random.Intn(5); r {
			case 0:
				fmt.Printf("I: (%s) simulating a crash\n", identity)
				return
			case 1:
				fmt.Printf("I: (%s) simulating CPU overload\n", identity)
				time.Sleep(3 * time.Second)
			}
		}

		fmt.Printf("I: (%s) normal reply\n", identity)
		time.Sleep(1 * time.Second) //  Do some heavy work
		worker.SendMultipart(msg, 0)
	}
}
