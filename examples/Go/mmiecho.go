//
// MMI echo query example
//
// Author: amyangfei <amyangfei@gmail.com>
// Requires: http://github.com/alecthomas/gozmq

package main

import (
    "fmt"
    "os"
)

func main() {
    verbose := len(os.Args) > 1 && os.Args[1] == "-v"
    client := NewClient("tcp://localhost:5555", verbose)
    defer client.Close()

    // This is the service we send our request to
    reply := client.Send([]byte("mmi.service"), [][]byte{[]byte("echo")})

    fmt.Println("here")
    if len(reply) > 0 {
        reply_code := reply[0]
        fmt.Printf("Lookup echo service: %s\n", reply_code)
    } else {
        fmt.Printf("E: no response from broker, make sure it's running\n")
    }
}
