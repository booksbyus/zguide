//  Lazy Pirate client
//  Use zmq_poll to do a safe request-reply
//  To run, start lpserver and then randomly kill/restart it
//
//  Author: iano <scaly.iano@gmail.com>
//  Based on C example

package main

import (
	"fmt"
	zmq "github.com/alecthomas/gozmq"
	"strconv"
)

const (
	REQUEST_TIMEOUT = 2500 * 1000
	REQUEST_RETRIES = 3
	SERVER_ENDPOINT = "tcp://localhost:5555"
)

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()

	fmt.Println("I: Connecting to server...")
	client, _ := context.NewSocket(zmq.REQ)

	client.Connect(SERVER_ENDPOINT)

	for sequence, retriesLeft := 1, REQUEST_RETRIES; retriesLeft > 0; sequence++ {
		fmt.Printf("I: Sending (%d)\n", sequence)
		client.Send([]byte(strconv.Itoa(sequence)), 0)

		for expectReply := true; expectReply; {
			//  Poll socket for a reply, with timeout
			items := zmq.PollItems{
				zmq.PollItem{Socket: client, Events: zmq.POLLIN},
			}
			if _, err := zmq.Poll(items, REQUEST_TIMEOUT); err != nil {
				panic(err) //  Interrupted
			}

			//  .split process server reply
			//  Here we process a server reply and exit our loop if the
			//  reply is valid. If we didn't a reply we close the client
			//  socket and resend the request. We try a number of times
			//  before finally abandoning:

			if item := items[0]; item.REvents&zmq.POLLIN != 0 {
				//  We got a reply from the server, must match sequence
				reply, err := item.Socket.Recv(0)
				if err != nil {
					panic(err) //  Interrupted
				}

				if replyInt, err := strconv.Atoi(string(reply)); replyInt == sequence && err == nil {
					fmt.Printf("I: Server replied OK (%s)\n", reply)
					retriesLeft = REQUEST_RETRIES
					expectReply = false
				} else {
					fmt.Printf("E: Malformed reply from server: %s", reply)
				}
			} else if retriesLeft--; retriesLeft == 0 {
				fmt.Println("E: Server seems to be offline, abandoning")
				client.SetLinger(0)
				client.Close()
				break
			} else {
				fmt.Println("W: No response from server, retrying...")
				//  Old socket is confused; close it and open a new one
				client.SetLinger(0)
				client.Close()
				client, _ = context.NewSocket(zmq.REQ)
				client.Connect(SERVER_ENDPOINT)
				fmt.Printf("I: Resending (%d)\n", sequence)
				//  Send request again, on new socket
				client.Send([]byte(strconv.Itoa(sequence)), 0)
			}
		}
	}
}
