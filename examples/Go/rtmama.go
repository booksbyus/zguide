//
//  Custom routing Router to Mama (ROUTER to REQ)
//

package main

import (
	zmq "github.com/alecthomas/gozmq"
	"math/rand"
	"time"
	"fmt"
	"strings"
)

const NBR_WORKERS = 10

func randomString() string {
  source := "abcdefghijklmnopqrstuvwxyz"
  target := make([]string, 20)
  for i := 0; i < 20; i++ {
	target[i] = string(source[rand.Intn(len(source))])
  }
  return strings.Join(target, "")
}

func workerTask(randomId string) {
  context, _ := zmq.NewContext()
  defer context.Close()

  worker, _ := context.NewSocket(zmq.REQ)
  /*worker.SetSockOptString(zmq.IDENTITY, randomString())*/
  worker.SetSockOptString(zmq.IDENTITY, randomId)

  worker.Connect("ipc://routing.ipc")
  defer worker.Close()

  var total int
  for {
	err := worker.Send([]byte("ready"), 0)
	if err != nil { print(err) }
	data, _ := worker.Recv(0)
	if string(data) == "END" {
	  id, _ := worker.GetSockOptString(zmq.IDENTITY)
	  fmt.Printf("Processed: %2d tasks (%s)\n", total, id)
	}
	total += 1

	/*msec := rand.Intn(1000)*/
	/*time.Sleep(time.Duration(msec) * time.Millisecond)*/
  }
}

//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each thread has its own
//  context and conceptually acts as a separate process.

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()

	client, _ := context.NewSocket(zmq.ROUTER)
	defer client.Close()
	client.Bind("ipc://routing.ipc")

	rand.Seed(time.Now().Unix())

	for i := 0; i < NBR_WORKERS; i++ {
	  go workerTask(randomString())
	}

	assignments := make(map[string]int)
	for i := 0; i < NBR_WORKERS * 10; i++ {
	  //  LRU worker is next waiting in queue
	  parts, err := client.RecvMultipart(0)
	  if err != nil { print(err) }
	  address := parts[0]
	  client.SendMultipart([][]byte{address, []byte(""), []byte("This is the workload")}, 0)
	  assignments[string(address)] += 1
	}
	fmt.Println(assignments)

	//  Now ask mamas to shut down and report their results
	for i := 0; i < NBR_WORKERS; i++ {
	  parts, err := client.RecvMultipart(0)
	  if err != nil { print(err) }
	  address := parts[0]
	  client.SendMultipart([][]byte{address, []byte(""), []byte("END")}, 0)
	}
}
