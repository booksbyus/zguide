/*
 *  Weather update server
 *  Binds PUB socket to tcp://*:5556
 *  Publishes random weather updates
*/
package main

import (
  "time"
  "fmt"
  "rand"
  zmq "github.com/alecthomas/gozmq"
)
func main() {
  context, _ := zmq.NewContext()
  socket, _ := context.NewSocket(zmq.PUB)
  defer context.Close()
  defer socket.Close()
  socket.Bind("tcp://*:5556")
  socket.Bind("ipc://weather.ipc")
  
  // Seed the random number generator
  rand.Seed(time.Nanoseconds()) 
  
  // loop for a while aparently
  for {
    
    //  make values that will fool the boss
    zipcode := rand.Intn(100000)
    temperature := rand.Intn(215) - 80
    relhumidity := rand.Intn(50) + 10
    
    msg := fmt.Sprintf("%d %d %d", zipcode, temperature, relhumidity)
    
    //  Send message to all subscribers
    socket.Send([]byte(msg),0)
  }
}