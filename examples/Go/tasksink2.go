//
// Task sink
// Binds PULL socket to tcp://localhost:5558
// Collects results from workers via that socket
//
package main

import (
	"fmt"
	zmq "github.com/alecthomas/gozmq"
	"time"
)

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()

	//  Socket to receive messages on
	receiver, _ := context.NewSocket(zmq.PULL)
	defer receiver.Close()
	receiver.Bind("tcp://*:5558")

	//  Socket for worker control
	controller, _ := context.NewSocket(zmq.PUB)
	defer controller.Close()
	controller.Bind("tcp://*:5559")

	//  Wait for start of batch
	msgbytes, _ := receiver.Recv(0)
	fmt.Println("Received Start Msg ", string(msgbytes))

	//  Start our clock now
	start_time := time.Now().UnixNano()

	for i := 0; i < 100; i++ {
		msgbytes, _ = receiver.Recv(0)
		if i%10 == 0 {
			fmt.Print(":")
		} else {
			fmt.Print(".")
		}
	}

	//  Calculate and report duration of batch
	te := time.Now().UnixNano()
	fmt.Printf("Total elapsed time: %d msec\n", (te-start_time)/1e6)

	err := controller.Send([]byte("KILL"), 0)
	if err != nil {
		fmt.Println(err)
	}
	time.Sleep(1 * time.Second)
}
