//
//  Custom routing Router to Mama (ROUTER to REQ)
//

package main

import (
	zmq "github.com/alecthomas/gozmq"
	"math/rand"
	"time"
)

const NBR_WORKERS = 10

func (socket *zmq.Socket) setId() {
}

func workerTask() {
  context, _ := zmq.NewContext()
  defer context.Close()

  worker, _ := context.NewSocket(zmq.REQ)
  defer worker.Close()
  
  /*worker.SetSockOptString(zmq.IDENTITY, )*/
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

	print("spinning up workers")
	for i := 0; i < NBR_WORKERS; i++ {
	  go workerTask()
	}

	print("send work")
	for i := 0; i < NBR_WORKERS; i++ {
	  //  LRU worker is next waiting in queue
	  parts, err := client.RecvMultipart(0)
	  if err != nil { print(err) }
	  address := parts[0]
	  client.SendMultipart([][]byte{address, []byte("This is the workload")}, 0)
	}

	print("shutdown")
	//  Now ask mamas to shut down and report their results
	for i := 0; i < NBR_WORKERS; i++ {
	  parts, err := client.RecvMultipart(0)
	  if err != nil { print(err) }
	  address := parts[0]
	  client.SendMultipart([][]byte{address, []byte("END")}, 0)
	}
}
