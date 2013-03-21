//  Majordomo Protocol worker example
//  Uses the mdwrk API to hide all MDP aspects
//
//  To run this example, you may need to run multiple *.go files as below
//  go run mdp.go zhelpers.go mdwrkapi.go mdworker.go [-v]
//
//  Author: iano <scaly.iano@gmail.com>

package main

import (
	"os"
)

func main() {
	verbose := len(os.Args) >= 2 && os.Args[1] == "-v"

	worker := NewWorker("tcp://localhost:5555", "echo", verbose)
	for reply := [][]byte{}; ; {
		request := worker.Recv(reply)
		if len(request) == 0 {
			break
		}
		reply = request
	}
}
