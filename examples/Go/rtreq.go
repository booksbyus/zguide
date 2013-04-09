//
//  ROUTER-to-REQ example
//

package main

import (
	"fmt"
	zmq "github.com/alecthomas/gozmq"
	"math/rand"
	"strings"
	"time"
)

const NBR_WORKERS = 10

func randomString() string {
	source := "abcdefghijklmnopqrstuvwxyz"
	target := make([]string, 20)
	for i := 0; i < 20; i++ {
		target[i] = string(source[rand.Intn(len(source))])
	}
	return strings.Join(target, "")
}

func workerTask() {
	context, _ := zmq.NewContext()
	defer context.Close()

	worker, _ := context.NewSocket(zmq.REQ)
	worker.SetIdentity(randomString())

	worker.Connect("tcp://localhost:5671")
	defer worker.Close()

	total := 0
	for {
		err := worker.Send([]byte("Hi Boss"), 0)
		if err != nil {
			print(err)
		}
		workload, _ := worker.Recv(0)
		if string(workload) == "Fired!" {
			id, _ := worker.Identity()
			fmt.Printf("Completed: %d tasks (%s)\n", total, id)
			break
		}

		total += 1
		msec := rand.Intn(1000)
		time.Sleep(time.Duration(msec) * time.Millisecond)
	}
}

//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each goroutine has its own
//  context and conceptually acts as a separate process.

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()

	broker, _ := context.NewSocket(zmq.ROUTER)
	defer broker.Close()
	broker.Bind("tcp://*:5671")

	rand.Seed(time.Now().Unix())

	for i := 0; i < NBR_WORKERS; i++ {
		go workerTask()
	}

	end_time := time.Now().Unix() + 5
	workers_fired := 0

	for {
		//  Next message gives us least recently used worker
		parts, err := broker.RecvMultipart(0)
		if err != nil {
			print(err)
		}
		identity := parts[0]
		now := time.Now().Unix()
		if now < end_time {
			broker.SendMultipart([][]byte{identity, []byte(""), []byte("Work harder")}, 0)
		} else {
			broker.SendMultipart([][]byte{identity, []byte(""), []byte("Fired!")}, 0)
			workers_fired++
			if workers_fired == NBR_WORKERS {
				break
			}
		}
	}
}
