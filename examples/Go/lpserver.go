//  Lazy Pirate server
//  Binds REQ socket to tcp://*:5555
//  Like hwserver except:
//   - echoes request as-is
//   - randomly runs slowly, or exits to simulate a crash.
//
//  Author: iano <scaly.iano@gmail.com>
//  Based on C example

package main

import (
	"fmt"
	zmq "github.com/alecthomas/gozmq"
	"math/rand"
	"time"
)

const (
	SERVER_ENDPOINT = "tcp://*:5555"
)

func main() {
	src := rand.NewSource(time.Now().UnixNano())
	random := rand.New(src)

	context, _ := zmq.NewContext()
	defer context.Close()

	server, _ := context.NewSocket(zmq.REP)
	defer server.Close()
	server.Bind(SERVER_ENDPOINT)

	for cycles := 1; ; cycles++ {
		request, _ := server.Recv(0)

		//  Simulate various problems, after a few cycles
		if cycles > 3 {
			switch r := random.Intn(3); r {
			case 0:
				fmt.Println("I: Simulating a crash")
				return
			case 1:
				fmt.Println("I: simulating CPU overload")
				time.Sleep(2 * time.Second)
			}
		}
		fmt.Printf("I: normal request (%s)\n", request)
		time.Sleep(1 * time.Second) //  Do some heavy work
		server.Send(request, 0)
	}
}
