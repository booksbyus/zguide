//
//   Weather update server
//   Binds PUB socket to tcp://*:5556
//   Publishes random weather updates
//   Initial Commit in examples/Go by Aaron Raddon
//   Ported to goczmq by Michael Guldan github.com/michaelcoyote
//   Requires: http://github.com/zeromq/goczmq
//
package main

import (
	"fmt"
	goczmq "github.com/zeromq/goczmq"
	"math/rand"
	"time"
)

func main() {
	socket := goczmq.NewSock(goczmq.Pub)
	defer socket.Destroy()
	socket.Bind("tcp://*:5556")
	socket.Bind("ipc://weather.ipc")

	// Seed the random number generator
	rand.Seed(time.Now().UnixNano())

	// loop for a while aparently
	for {

		//  make values that will fool the boss
		zipcode := rand.Intn(100000)
		temperature := rand.Intn(215) - 80
		relhumidity := rand.Intn(50) + 10

		msg := []byte(fmt.Sprintf("%d %d %d", zipcode, temperature, relhumidity))

		//  Send message to all subscribers
		socket.SendMessage([][]byte{msg})
	}
}
