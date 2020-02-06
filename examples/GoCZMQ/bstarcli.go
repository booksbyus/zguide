package main

import (
	"encoding/binary"
	"fmt"
	zmq "github.com/zeromq/goczmq"
	"time"
)

const (
	REQUEST_TIMEOUT = 1000 //msecs
	SETTLE_DELAY    = 2000 // before falling over
)

func main() {
	server := []string{"tcp://localhost:5001", "tcp://localhost:5002"}
	serverNumber := 0
	fmt.Printf("Connecting to server at %s", server[serverNumber])
	client, _ := zmq.NewReq(server[serverNumber])
	defer client.Destroy()

	var sequence uint32 = 0

	for {
		sequence = sequence + 1
		bs := make([]byte, 4)
		binary.LittleEndian.PutUint32(bs, sequence)
		//we send a request, then we work to get a reply
		msgToSend := [][]byte{bs}
		client.SendMessage(msgToSend)

		expectReply := true
		poller, _ := zmq.NewPoller(client)
		for expectReply {
			sock := poller.Wait(REQUEST_TIMEOUT)
			//if sock == nil {
			//	break // Interrupted
			//}

			//  We use a Lazy Pirate strategy in the client. If there's no
			//  reply within our timeout, we close the socket and try again.
			//  In Binary Star, it's the client vote that decides which
			//  server is primary; the client must therefore try to connect
			//  to each server in turn:
			if sock == client {
				// we got a reply from the server, must match sequence
				message, _ := client.RecvMessage()
				seq := binary.LittleEndian.Uint32(message[0])
				if seq == sequence {
					fmt.Printf("Server replied ok (%d)\n", seq)
					expectReply = false
					time.Sleep(1 * time.Second) // One request per second.
				} else {
					fmt.Printf("Bad reply from server (%d)\n", seq)
				}
			} else {
				fmt.Printf("No response from server, failing over\n")
				// old socket is confused, close it and open a new one
				client.Destroy()
				serverNumber = (serverNumber + 1) % 2
				time.Sleep(SETTLE_DELAY * time.Millisecond)
				fmt.Printf("Connecting to server at %s\n", server[serverNumber])
				client, _ = zmq.NewReq(server[serverNumber])
				poller, _ = zmq.NewPoller(client)
				// send request again, on new socket.
				client.SendMessage(msgToSend)
			}
		}
	}
}
