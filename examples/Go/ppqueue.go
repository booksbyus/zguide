//  Paranoid Pirate queue
//
//  Author: iano <scaly.iano@gmail.com>
//  Based on C & Python example

package main

import (
	"container/list"
	"fmt"
	zmq "github.com/alecthomas/gozmq"
	"time"
)

const (
	HEARTBEAT_INTERVAL = time.Second //  time.Duration

	//  Paranoid Pirate Protocol constants
	PPP_READY     = "\001" //  Signals worker is ready
	PPP_HEARTBEAT = "\002" //  Signals worker heartbeat
)

type PPWorker struct {
	address []byte    //  Address of worker
	expiry  time.Time //  Expires at this time
}

func NewPPWorker(address []byte) *PPWorker {
	return &PPWorker{
		address: address,
		expiry:  time.Now().Add(HEARTBEAT_LIVENESS * HEARTBEAT_INTERVAL),
	}
}

type WorkerQueue struct {
	queue *list.List
}

func NewWorkerQueue() *WorkerQueue {
	return &WorkerQueue{
		queue: list.New(),
	}
}

func (workers *WorkerQueue) Len() int {
	return workers.queue.Len()
}

func (workers *WorkerQueue) Next() []byte {
	elem := workers.queue.Back()
	worker, _ := elem.Value.(*PPWorker)
	workers.queue.Remove(elem)
	return worker.address
}

func (workers *WorkerQueue) Ready(worker *PPWorker) {
	for elem := workers.queue.Front(); elem != nil; elem = elem.Next() {
		if w, _ := elem.Value.(*PPWorker); string(w.address) == string(worker.address) {
			workers.queue.Remove(elem)
			break
		}
	}
	workers.queue.PushBack(worker)
}

func (workers *WorkerQueue) Purge() {
	now := time.Now()
	for elem := workers.queue.Front(); elem != nil; elem = workers.queue.Front() {
		if w, _ := elem.Value.(*PPWorker); w.expiry.After(now) {
			break
		}
		workers.queue.Remove(elem)
	}
}

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()

	frontend, _ := context.NewSocket(zmq.ROUTER)
	defer frontend.Close()
	frontend.Bind("tcp://*:5555") //  For clients

	backend, _ := context.NewSocket(zmq.ROUTER)
	defer backend.Close()
	backend.Bind("tcp://*:5556") //  For workers

	workers := NewWorkerQueue()
	heartbeatAt := time.Now().Add(HEARTBEAT_INTERVAL)

	for {
		items := zmq.PollItems{
			zmq.PollItem{Socket: backend, Events: zmq.POLLIN},
			zmq.PollItem{Socket: frontend, Events: zmq.POLLIN},
		}

		//  Poll frontend only if we have available workers
		if workers.Len() > 0 {
			zmq.Poll(items, HEARTBEAT_INTERVAL)
		} else {
			zmq.Poll(items[:1], HEARTBEAT_INTERVAL)
		}

		//  Handle worker activity on backend
		if items[0].REvents&zmq.POLLIN != 0 {
			frames, err := backend.RecvMultipart(0)
			if err != nil {
				panic(err) //  Interrupted
			}
			address := frames[0]
			workers.Ready(NewPPWorker(address))

			//  Validate control message, or return reply to client
			if msg := frames[1:]; len(msg) == 1 {
				switch status := string(msg[0]); status {
				case PPP_READY:
					fmt.Println("I: PPWorker ready")
				case PPP_HEARTBEAT:
					fmt.Println("I: PPWorker heartbeat")
				default:
					fmt.Println("E: Invalid message from worker: ", msg)
				}
			} else {
				frontend.SendMultipart(msg, 0)
			}
		}

		if items[1].REvents&zmq.POLLIN != 0 {
			//  Now get next client request, route to next worker
			frames, err := frontend.RecvMultipart(0)
			if err != nil {
				panic(err)
			}
			frames = append([][]byte{workers.Next()}, frames...)
			backend.SendMultipart(frames, 0)
		}

		//  .split handle heartbeating
		//  We handle heartbeating after any socket activity. First we send
		//  heartbeats to any idle workers if it's time. Then we purge any
		//  dead workers:
		if heartbeatAt.Before(time.Now()) {
			for elem := workers.queue.Front(); elem != nil; elem = elem.Next() {
				w, _ := elem.Value.(*PPWorker)
				msg := [][]byte{w.address, []byte(PPP_HEARTBEAT)}
				backend.SendMultipart(msg, 0)
			}
			heartbeatAt = time.Now().Add(HEARTBEAT_INTERVAL)
		}

		workers.Purge()
	}
}
