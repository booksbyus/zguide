//
//   Weather proxy listens to weather server which is constantly
//   emitting weather data
//   Binds SUB socket to tcp://*:5556
//   Initial Commit in examples/Go bay Aaron Raddon
//   Ported to goczmq by Michael Guldan github.com/michaelcoyote
//   Requires: http://github.com/zeromq/goczmq
//
package main

import (
	"fmt"
	goczmq "github.com/zeromq/goczmq"
	"os"
	"strconv"
	"strings"
)

func main() {
	socket := goczmq.NewSock(goczmq.Sub)
	defer socket.Destroy()

	var temps []string
	var err error
	var temp int64
	total_temp := 0
	filter := "59937"

	// find zipcode
	if len(os.Args) > 1 { // ./wuclient 85678
		filter = string(os.Args[1])
	}

	//  Subscribe to just one zipcode (whitefish MT 59937)
	fmt.Printf("Collecting updates from weather server for %s…\n", filter)
	socket.SetSubscribe(filter)
	socket.Connect("tcp://localhost:5556")

	for i := 0; i < 101; i++ {
		// found temperature point
		datapt, _ := socket.RecvMessage()
		temps = strings.Split(string(datapt[0]), " ")
		temp, err = strconv.ParseInt(temps[1], 10, 64)
		if err == nil {
			// Invalid string
			total_temp += int(temp)
		}
	}

	fmt.Printf("Average temperature for zipcode %s was %dF \n\n", filter, total_temp/100)
}
