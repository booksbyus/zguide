//  Simple Pirate broker
//  This is identical to load-balancing pattern, with no reliability
//  mechanisms. It depends on the client for recovery. Runs forever.
//
//  Author: iano <scaly.iano@gmail.com>
//  Based on C & Python example

package main

import (
	zmq "github.com/alecthomas/gozmq"
)

const LRU_READY = "\001"

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()

	frontend, _ := context.NewSocket(zmq.ROUTER)
	defer frontend.Close()
	frontend.Bind("tcp://*:5555") //  For clients

	backend, _ := context.NewSocket(zmq.ROUTER)
	defer backend.Close()
	backend.Bind("tcp://*:5556") //  For workers

	//  Queue of available workers
	workers := make([][]byte, 0, 0)

	for {
		items := zmq.PollItems{
			zmq.PollItem{Socket: backend, Events: zmq.POLLIN},
			zmq.PollItem{Socket: frontend, Events: zmq.POLLIN},
		}

		//  Poll frontend only if we have available workers
		if len(workers) > 0 {
			zmq.Poll(items, -1)
		} else {
			zmq.Poll(items[:1], -1)
		}

		//  Handle worker activity on backend
		if items[0].REvents&zmq.POLLIN != 0 {
			//  Use worker identity for load-balancing
			msg, err := backend.RecvMultipart(0)
			if err != nil {
				panic(err) //  Interrupted
			}
			address := msg[0]
			workers = append(workers, address)

			//  Forward message to client if it's not a READY
			if reply := msg[2:]; string(reply[0]) != LRU_READY {
				frontend.SendMultipart(reply, 0)
			}
		}

		if items[1].REvents&zmq.POLLIN != 0 {
			//  Get client request, route to first available worker
			msg, err := frontend.RecvMultipart(0)
			if err != nil {
				panic(err) //  Interrupted
			}
			last := workers[len(workers)-1]
			workers = workers[:len(workers)-1]
			request := append([][]byte{last, nil}, msg...)
			backend.SendMultipart(request, 0)
		}
	}
}
