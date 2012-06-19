package main

import (
	"fmt"
	zmq "github.com/alecthomas/gozmq"
	"os"
	"os/signal"
)

func main() {
	signal_channel := make(chan os.Signal)
	signal.Notify(signal_channel)

	go func() {

		context, _ := zmq.NewContext()
		defer context.Close()

		socket, _ := context.NewSocket(zmq.REP)
		defer socket.Close()
		socket.Bind("tcp://*:5555")

		msgbytes, err := socket.Recv(0)
		if err != nil {
			fmt.Println(err)
		}
		fmt.Printf("%s.\n", string(msgbytes))
	}()

	<-signal_channel
	fmt.Println("exiting")
	os.Exit(0)
}
