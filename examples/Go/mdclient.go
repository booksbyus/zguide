//  Majordomo Protocol client example
//  Uses the mdcli API to hide all MDP aspects
//  
//  To run this example, you may need to run multiple *.go files as below
//  go run mdp.go zhelpers.go mdcliapi.go mdclient.go [-v]
//
//  Author: iano <scaly.iano@gmail.com>

package main

import (
	"fmt"
	"os"
)

func main() {
	verbose := len(os.Args) >= 2 && os.Args[1] == "-v"

	client := NewClient("tcp://localhost:5555", verbose)
	defer client.Close()

	count := 0
	for ; count < 1e5; count++ {
		request := [][]byte{[]byte("Hello world")}
		reply := client.Send([]byte("echo"), request)
		if len(reply) == 0 {
			break
		}
	}

	fmt.Printf("%d requests/replies processed\n", count)
}
