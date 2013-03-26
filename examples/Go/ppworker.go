//  Paranoid Pirate worker
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

const (
	HEARTBEAT_INTERVAL = time.Second //  time.Duration

	INTERVAL_INIT = time.Second      //  Initial reconnect
	INTERVAL_MAX  = 32 * time.Second //  After exponential backoff

	//  Paranoid Pirate Protocol constants
	PPP_READY     = "\001" //  Signals worker is ready
	PPP_HEARTBEAT = "\002" //  Signals worker heartbeat
)

//  Helper function that returns a new configured socket
//  connected to the Paranoid Pirate queue

func WorkerSocket(context *zmq.Context) *zmq.Socket {
	worker, _ := context.NewSocket(zmq.DEALER)
	worker.Connect("tcp://localhost:5556")

	//  Tell queue we're ready for work
	fmt.Println("I: worker ready")
	worker.Send([]byte(PPP_READY), 0)
	return worker
}

//  .split main task
//  We have a single task, which implements the worker side of the
//  Paranoid Pirate Protocol (PPP). The interesting parts here are
//  the heartbeating, which lets the worker detect if the queue has
//  died, and vice-versa:

func main() {
	src := rand.NewSource(time.Now().UnixNano())
	random := rand.New(src)

	context, _ := zmq.NewContext()
	defer context.Close()

	worker := WorkerSocket(context)

	liveness := HEARTBEAT_LIVENESS
	interval := INTERVAL_INIT
	heartbeatAt := time.Now().Add(HEARTBEAT_INTERVAL)
	cycles := 0
	for {
		items := zmq.PollItems{
			zmq.PollItem{Socket: worker, Events: zmq.POLLIN},
		}

		zmq.Poll(items, HEARTBEAT_INTERVAL)

		if items[0].REvents&zmq.POLLIN != 0 {
			frames, err := worker.RecvMultipart(0)
			if err != nil {
				panic(err)
			}

			if len(frames) == 3 {
				cycles++
				if cycles > 3 {
					switch r := random.Intn(5); r {
					case 0:
						fmt.Println("I: Simulating a crash")
						worker.Close()
						return
					case 1:
						fmt.Println("I: Simulating CPU overload")
						time.Sleep(3 * time.Second)
					}
				}
				fmt.Println("I: Normal reply")
				worker.SendMultipart(frames, 0)
				liveness = HEARTBEAT_LIVENESS
				time.Sleep(1 * time.Second)
			} else if len(frames) == 1 && string(frames[0]) == PPP_HEARTBEAT {
				fmt.Println("I: Queue heartbeat")
				liveness = HEARTBEAT_LIVENESS
			} else {
				fmt.Println("E: Invalid message")
			}
			interval = INTERVAL_INIT
		} else if liveness--; liveness == 0 {
			fmt.Println("W: Heartbeat failure, can't reach queue")
			fmt.Printf("W: Reconnecting in %0.2fs...", interval)
			time.Sleep(interval)

			if interval < INTERVAL_MAX {
				interval *= 2
			}
			worker.Close()
			worker = WorkerSocket(context)
			liveness = HEARTBEAT_LIVENESS
		}

		if heartbeatAt.Before(time.Now()) {
			heartbeatAt = time.Now().Add(HEARTBEAT_INTERVAL)
			worker.Send([]byte(PPP_HEARTBEAT), 0)
		}
	}

}
