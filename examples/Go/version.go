//
// 0MQ version example.
//
// Author: Max Riveiro <kavu13@gmail.com>
// Requires: http://github.com/alecthomas/gozmq
//
package main

import (
  "fmt"
  zmq "github.com/alecthomas/gozmq"
)

func main() {
  major, minor, patch := zmq.Version()
  fmt.Printf("Current 0MQ version is %d.%d.%d\n", major, minor, patch)
}
