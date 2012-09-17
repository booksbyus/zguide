//
//  Custom routing Router to Dealer
//
//  We have two workers, here we copy the code, normally these would
//  run on different boxes…
//

package main

import (
  zmq "github.com/alecthomas/gozmq"
  "time"
  "math/rand"
)

func workerTaskA() {
}

func workerTaskB() {
}

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()
	client, _ := context.NewSocket(zmq.ROUTER)
	defer client.Close()

	go workerTaskA()
	go workerTaskB()

	//  Wait for threads to connect, since otherwise the messages
	//  we send won't be routable.
	time.Sleep(time.Second)

	//  Send 10 tasks scattered to A twice as often as B
	for i := 0; i < 10; i++ {
		parts := make([][]byte, 0)
		if rand.Intn(10) < 3 {
		  parts = append(parts, []byte("A"))
		} else {
		  parts = append(parts, []byte("B"))
		}
		parts = append(parts, []byte("This is the workload"))
		client.SendMultipart(parts, 0)
	}
	client.SendMultipart([][]byte{[]byte("A"), []byte("END")}, 0)
	client.SendMultipart([][]byte{[]byte("B"), []byte("END")}, 0)
}
