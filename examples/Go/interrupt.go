//
//  Shows how to handle Ctrl-C
//
package main

import (
  	"os/signal"
	"os"
	"fmt"
	/*"time"*/
	zmq "github.com/alecthomas/gozmq"
)

func listenForSignals(exit_channel chan bool) {
	signal_channel := make(chan os.Signal)
	signal.Notify(signal_channel)
	for {
		<- signal_channel
		fmt.Println("stopping")
		exit_channel <- true
	}
}

func main() {
	exit := make(chan bool)
	exit_signal := false
	go listenForSignals(exit)

	context, _ := zmq.NewContext()
	defer context.Close()

	socket, _ := context.NewSocket(zmq.REP)
	defer socket.Close()
	socket.Bind("tcp://*:5555")

	for exit_signal == false {
	  select {
	  case exit_signal = <- exit:
		fmt.Println("W: interrupt received, killing server...")
	  default:
		msgbytes, err := socket.Recv(zmq.NOBLOCK)
		if err != nil {
		  fmt.Print(err)
		}
		fmt.Printf("%s.\n", string(msgbytes))
	  }
	}

}
