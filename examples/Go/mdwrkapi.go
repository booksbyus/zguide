//  mdwrkapi class - Majordomo Protocol Worker API
//  Implements the MDP/Worker spec at http://rfc.zeromq.org/spec:7.
//
//  Author: iano <scaly.iano@gmail.com>
//  Based on C & Python example

package main

import (
	zmq "github.com/alecthomas/gozmq"
	"log"
	"time"
)

type Worker interface {
	Close()
	Recv([][]byte) [][]byte
}

type mdWorker struct {
	broker  string
	context *zmq.Context
	service string
	verbose bool
	worker  *zmq.Socket

	heartbeat   time.Duration
	heartbeatAt time.Time
	liveness    int
	reconnect   time.Duration

	expectReply bool
	replyTo     []byte
}

func NewWorker(broker, service string, verbose bool) Worker {
	context, _ := zmq.NewContext()
	self := &mdWorker{
		broker:    broker,
		context:   context,
		service:   service,
		verbose:   verbose,
		heartbeat: 2500 * time.Millisecond,
		liveness:  0,
		reconnect: 2500 * time.Millisecond,
	}
	self.reconnectToBroker()
	return self
}

func (self *mdWorker) reconnectToBroker() {
	if self.worker != nil {
		self.worker.Close()
	}
	self.worker, _ = self.context.NewSocket(zmq.DEALER)
	self.worker.SetLinger(0)
	self.worker.Connect(self.broker)
	if self.verbose {
		log.Printf("I: connecting to broker at %s...\n", self.broker)
	}
	self.sendToBroker(MDPW_READY, []byte(self.service), nil)
	self.liveness = HEARTBEAT_LIVENESS
	self.heartbeatAt = time.Now().Add(self.heartbeat)
}

func (self *mdWorker) sendToBroker(command string, option []byte, msg [][]byte) {
	if len(option) > 0 {
		msg = append([][]byte{option}, msg...)
	}

	msg = append([][]byte{nil, []byte(MDPW_WORKER), []byte(command)}, msg...)
	if self.verbose {
		log.Printf("I: sending %X to broker\n", command)
		Dump(msg)
	}
	self.worker.SendMultipart(msg, 0)
}

func (self *mdWorker) Close() {
	if self.worker != nil {
		self.worker.Close()
	}
	self.context.Close()
}

func (self *mdWorker) Recv(reply [][]byte) (msg [][]byte) {
	//  Format and send the reply if we were provided one

	if len(reply) == 0 && self.expectReply {
		panic("Error reply")
	}

	if len(reply) > 0 {
		if len(self.replyTo) == 0 {
			panic("Error replyTo")
		}
		reply = append([][]byte{self.replyTo, nil}, reply...)
		self.sendToBroker(MDPW_REPLY, nil, reply)
	}

	self.expectReply = true

	for {
		items := zmq.PollItems{
			zmq.PollItem{Socket: self.worker, Events: zmq.POLLIN},
		}

		_, err := zmq.Poll(items, self.heartbeat)
		if err != nil {
			panic(err) //  Interrupted
		}

		if item := items[0]; item.REvents&zmq.POLLIN != 0 {
			msg, _ = self.worker.RecvMultipart(0)
			if self.verbose {
				log.Println("I: received message from broker: ")
				Dump(msg)
			}
			self.liveness = HEARTBEAT_LIVENESS
			if len(msg) < 3 {
				panic("Invalid msg") //  Interrupted
			}

			header := msg[1]
			if string(header) != MDPW_WORKER {
				panic("Invalid header") //  Interrupted
			}

			switch command := string(msg[2]); command {
			case MDPW_REQUEST:
				//  We should pop and save as many addresses as there are
				//  up to a null part, but for now, just save one...
				self.replyTo = msg[3]
				msg = msg[5:]
				return
			case MDPW_HEARTBEAT:
				// do nothin
			case MDPW_DISCONNECT:
				self.reconnectToBroker()
			default:
				log.Println("E: invalid input message:")
				Dump(msg)
			}
		} else if self.liveness--; self.liveness <= 0 {
			if self.verbose {
				log.Println("W: disconnected from broker - retrying...")
			}
			time.Sleep(self.reconnect)
			self.reconnectToBroker()
		}

		//  Send HEARTBEAT if it's time
		if self.heartbeatAt.Before(time.Now()) {
			self.sendToBroker(MDPW_HEARTBEAT, nil, nil)
			self.heartbeatAt = time.Now().Add(self.heartbeat)
		}
	}

	return
}
