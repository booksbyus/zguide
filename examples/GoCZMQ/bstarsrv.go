package main

import (
	"encoding/binary"
	"flag"
	"fmt"
	zmq "github.com/zeromq/goczmq"
	"log"
	"os"
	"os/signal"
	"syscall"
	"time"
)

const (
	// heartbeat time in milliseconds  - this needs to be the same across active and backup servers
	HEARTBEAT = 1000
)

type State uint32

const (
	STATE_PRIMARY State = 1 // waiting for peer to connect
	STATE_BACKUP  State = 2 // waiting for peer to connect
	STATE_ACTIVE  State = 3 // accepting connections
	STATE_PASSIVE State = 4 // not accepting connections
)

type Event uint32

const (
	PEER_PRIMARY   Event = 1 // peer is pending primary
	PEER_BACKUP    Event = 2 // peer is pending backup
	PEER_ACTIVE    Event = 3 // peer is active
	PEER_PASSIVE   Event = 4 // peer is passive
	CLIENT_REQUEST Event = 5 // client makes request
)

type bstarsrv struct {
	state      State // current state
	event      Event // current event
	peerExpiry int64 // when peer is considered "dead"
}

//  The heart of the Binary Star design is its finite-state machine (FSM).
//  The FSM runs one event at a time. We apply an event to the current state,
//  which checks if the event is accepted, and if so, sets a new state:
func (b *bstarsrv) stateMachine() bool {
	exception := false

	//fmt.Printf("State %s Event %s\n", b.state, b.event)
	//  These are the PRIMARY and BACKUP states; we're waiting to become
	//  ACTIVE or PASSIVE depending on events we get from our peer:
	if b.state == STATE_PRIMARY {
		if b.event == PEER_BACKUP {
			fmt.Printf("I: connected to backup (passive), ready active\n")
			b.state = STATE_ACTIVE
		} else if b.event == PEER_ACTIVE {
			fmt.Printf("I: connected to backup (active), ready passive\n")
			b.state = STATE_PASSIVE
		}
		//  Accept client connections
	} else if b.state == STATE_BACKUP {
		if b.event == PEER_ACTIVE {
			fmt.Printf("I: connected to primary (active), ready passive\n")
			b.state = STATE_PASSIVE
		} else {
			//  Reject client connections when acting as backup
			if b.event == CLIENT_REQUEST {
				exception = true
			}
		}
	} else {
		//  These are the ACTIVE and PASSIVE states:
		if b.state == STATE_ACTIVE {
			if b.event == PEER_ACTIVE {
				//  Two actives would mean split-brain
				fmt.Printf("E: fatal error - dual actives, aborting\n")
				exception = true
			}
		} else {
			//  Server is passive
			//  CLIENT_REQUEST events can trigger failover if peer looks dead
			if b.state == STATE_PASSIVE {
				if b.event == PEER_PRIMARY {
					//  Peer is restarting - become active, peer will go passive
					fmt.Printf("I: primary (passive) is restarting, ready active\n")
					b.state = STATE_ACTIVE
				} else if b.event == PEER_BACKUP {
					//  Peer is restarting - become active, peer will go passive
					fmt.Printf("I: backup (passive) is restarting, ready active\n")
					b.state = STATE_ACTIVE
				} else if b.event == PEER_PASSIVE {
					//  Two passives would mean cluster would be non-responsive
					fmt.Printf("E: fatal error - dual passives, aborting\n")
					exception = true
				} else if b.event == CLIENT_REQUEST {
					//  Peer becomes active if timeout has passed
					//  It's the client request that triggers the failover
					//assert(peerExpiry > 0);
					if b.peerExpiry <= 0 {
						// freak out
						fmt.Printf("Unexpected peer expiry %d", b.peerExpiry)
					}

					if time.Now().UnixNano()/int64(time.Millisecond) >= b.peerExpiry {
						//  If peer is dead, switch to the active state
						fmt.Printf("I: failover successful, ready active\n")
						b.state = STATE_ACTIVE
					} else {
						//  If peer is alive, reject connections
						exception = true
					}
				}
			}
		}
	}
	return exception
}

func main() {
	primaryPtr := flag.Bool("p", false, "Startup as primary server")
	backupPtr := flag.Bool("b", false, "Startup as backup server")

	flag.Parse()

	fsm := &bstarsrv{}
	var frontend, statesub, statepub *zmq.Sock
	// ensure one, and only one, of the flags is set.
	if *primaryPtr && *backupPtr || !*primaryPtr && !*backupPtr {
		flag.Usage()
		os.Exit(1)
	} else if *primaryPtr {
		log.Println("Primary active, waiting for backup (passive)")
		frontend, _ = zmq.NewRouter("tcp://*:5001")
		statepub, _ = zmq.NewPub("tcp://*:5003")
		statesub, _ = zmq.NewSub("tcp://localhost:5004", "")
		fsm.state = STATE_PRIMARY
	} else {
		log.Println("Backup passive, waiting for primary (active)")
		frontend, _ = zmq.NewRouter("tcp://*:5002")
		statepub, _ = zmq.NewPub("tcp://*:5004")
		statesub, _ = zmq.NewSub("tcp://localhost:5003", "")
		fsm.state = STATE_BACKUP
	}

	defer frontend.Destroy()
	defer statepub.Destroy()
	defer statesub.Destroy()

	poller, _ := zmq.NewPoller(frontend, statesub)

	// set timer for next outgoing state message
	sendStateAt := time.Now().Add(HEARTBEAT * time.Millisecond)
	//fmt.Println("Now", time.Now())
	//fmt.Printf("sendStateAt %v\n", sendStateAt)

	interrupted := false
	c := make(chan os.Signal, 2)
	signal.Notify(c, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-c
		fmt.Println("\r- Ctrl+C pressed in Terminal")
		interrupted = true
	}()

	for !interrupted {
		timeLeft := int(time.Until(sendStateAt).Milliseconds())
		//fmt.Printf("Time left to poll %d\n", timeLeft)

		if timeLeft < 0 {
			timeLeft = 0
		}

		sock := poller.Wait(timeLeft)

		if sock != nil {
			//fmt.Println("Nothing on sockets.")
			//break // context has been shut down
			if sock == frontend {
				// have a client request
				message, _ := frontend.RecvMessage()
				fsm.event = CLIENT_REQUEST
				if !fsm.stateMachine() {
					// echo request back to client
					frontend.SendMessage(message)
				}
			} else {
				// state from our peer
				message, _ := statesub.RecvMessage()
				fsm.event = Event(binary.LittleEndian.Uint32(message[0]))
				if fsm.stateMachine() {
					fmt.Println("!stateMachine, exiting")
					break // Error, so exit
				}

				fsm.peerExpiry = time.Now().Add((2*HEARTBEAT)*time.Millisecond).UnixNano() / int64(time.Millisecond)
			}
		}

		// if we timed out send state to peer.
		if time.Until(sendStateAt) <= 0 {
			bs := make([]byte, 4)
			binary.LittleEndian.PutUint32(bs, uint32(fsm.state))
			statepub.SendMessage([][]byte{bs})
			sendStateAt = time.Now().Add(HEARTBEAT * time.Millisecond)
		}
	}

	fmt.Println("Interrupted")
}
