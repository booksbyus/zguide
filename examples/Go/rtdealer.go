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
)

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()
	client, _ := zmq.NewSocket(zmq.ROUTER)
	defer client.Close()

	go workerTaskA()
	go workerTaskB()

	//  Wait for threads to connect, since otherwise the messages
	//  we send won't be routable.
	time.Sleep(time.Second)

	//  Send 10 tasks scattered to A twice as often as B



}
