//
//  Custom routing Router to Dealer
//
//  We have two workers, here we copy the code, normally these would
//  run on different boxes…
//

package main

import (
	"fmt"
	zmq "github.com/alecthomas/gozmq"
	"math/rand"
	"time"
)

func workerTaskB() {
	context, _ := zmq.NewContext()
	defer context.Close()

	worker, _ := context.NewSocket(zmq.DEALER)
	defer worker.Close()
	worker.SetSockOptString(zmq.IDENTITY, "B")
	worker.Connect("ipc://routing.ipc")

	var total int
	for {
		//  We receive one part, with the workload
		request, _ := worker.Recv(0)
		if string(request) == "END" {
			fmt.Printf("B received: %d\n", total)
			break
		}
		total++
	}
}

func workerTaskA() {
	context, _ := zmq.NewContext()
	defer context.Close()

	worker, _ := context.NewSocket(zmq.DEALER)
	defer worker.Close()
	worker.SetSockOptString(zmq.IDENTITY, "A")
	worker.Connect("ipc://routing.ipc")

	var total int
	for {
		//  We receive one part, with the workload
		request, _ := worker.Recv(0)
		if string(request) == "END" {
			fmt.Printf("A received: %d\n", total)
			break
		}
		total++
	}
}

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()
	client, _ := context.NewSocket(zmq.ROUTER)
	defer client.Close()
	client.Bind("ipc://routing.ipc")

	go workerTaskA()
	go workerTaskB()

	//  Wait for threads to connect, since otherwise the messages
	//  we send won't be routable.
	time.Sleep(time.Second)

	//  Send 10 tasks scattered to A twice as often as B
	rand.Seed(time.Now().Unix())
	for i := 0; i < 10; i++ {
		parts := make([][]byte, 0)
		if rand.Intn(10) < 3 {
			parts = append(parts, []byte("A"))
		} else {
			parts = append(parts, []byte("B"))
		}
		parts = append(parts, []byte("This is the workload"))
		err := client.SendMultipart(parts, 0)
		if err != nil {
			fmt.Println(err)
		}
	}
	client.SendMultipart([][]byte{[]byte("A"), []byte("END")}, 0)
	client.SendMultipart([][]byte{[]byte("B"), []byte("END")}, 0)
}
