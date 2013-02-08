//  Majordomo Protocol broker
//  A minimal C implementation of the Majordomo Protocol as defined in
//  http://rfc.zeromq.org/spec:7 and http://rfc.zeromq.org/spec:8.
// 
//  To run this example, you may need to run multiple *.go files as below
//  go run mdp.go zhelpers.go mdbroker.go [-v]
//
//  Author: iano <scaly.iano@gmail.com>
//  Based on C & Python example

package main

import (
    "encoding/hex"
    "log"
    "os"
    "time"
    zmq "github.com/alecthomas/gozmq"
)

const (
    INTERNAL_SERVICE_PREFIX = "mmi."
    HEARTBEAT_LIVENESS = 3
    HEARTBEAT_INTERVAL = 2500 * time.Millisecond
    HEARTBEAT_EXPIRY = HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS
)

type Broker interface {
    Close()
    Run()
}

type mdWorker struct {
    identity string
    address []byte
    expiry time.Time
    service *mdService
}

type mdService struct {
    broker Broker
    name string
    requests [][][]byte
    waiting *ZList
}

type mdBroker struct {
    context zmq.Context
    heartbeatAt time.Time
    services map[string]*mdService
    socket zmq.Socket
    waiting *ZList
    workers map[string]*mdWorker
    verbose bool
}

func NewBroker(endpoint string, verbose bool) Broker {
    context, _ := zmq.NewContext()
    socket, _ := context.NewSocket(zmq.ROUTER)
    socket.SetSockOptInt(zmq.LINGER, 0)
    socket.Bind(endpoint)
    log.Printf("I: MDP broker/0.1.1 is active at %s\n", endpoint)
    return &mdBroker{
        context: context,
        heartbeatAt: time.Now().Add(HEARTBEAT_INTERVAL),
        services: make(map[string]*mdService),
        socket: socket,
        waiting: NewList(),
        workers: make(map[string]*mdWorker),
        verbose: verbose,
    }
}

func (self *mdBroker) deleteWorker(worker *mdWorker, disconnect bool) {
    if worker == nil { panic("Nil worker") }

    if disconnect {
        self.sendToWorker(worker, MDPW_DISCONNECT, nil, nil)
    }

    if worker.service != nil {
        worker.service.waiting.Delete(worker)
    }
    self.waiting.Delete(worker)
    delete(self.workers, worker.identity)
}

func (self *mdBroker) dispatch(service *mdService, msg [][]byte) {
    if service == nil { panic("Nil service") }
    if len(msg) != 0 {
        service.requests = append(service.requests, msg)
    }
    self.purgeWorkers()
    for service.waiting.Len() > 0 && len(service.requests) > 0 {
        msg, service.requests = service.requests[0], service.requests[1:]
        elem := service.waiting.Pop()
        self.waiting.Remove(elem)
        worker, _ := elem.Value.(*mdWorker)
        self.sendToWorker(worker, MDPW_REQUEST, nil, msg)
    }
}

func (self *mdBroker) processClient(sender []byte, msg [][]byte) {
    if len(msg) < 2 { panic("Invalid msg") }
    service := msg[0]
    msg = append([][]byte{sender, nil}, msg[1:]...)
    if string(service[:4]) == INTERNAL_SERVICE_PREFIX {
        self.serviceInternal(service, msg)
    } else {
        self.dispatch(self.requireService(string(service)), msg)
    }
}

func (self *mdBroker) processWorker(sender []byte, msg [][]byte) {
    if len(msg) < 1 { panic("Invalid msg") }

    command, msg := msg[0], msg[1:]
    identity := hex.EncodeToString(sender)
    worker, workerReady := self.workers[identity]
    if !workerReady {
        worker = &mdWorker{
            identity: identity,
            address: sender,
            expiry: time.Now().Add(HEARTBEAT_EXPIRY),
        }
        self.workers[identity] = worker
        if self.verbose {
            log.Printf("I: registering new worker: %s\n", identity)
        }
    }

    switch string(command) {
    case MDPW_READY:
        if len(msg) < 1 { panic("Invalid msg") }
        service := msg[0]
        if workerReady || string(service[:4]) == INTERNAL_SERVICE_PREFIX {
            self.deleteWorker(worker, true)
        } else {
            worker.service = self.requireService(string(service))
            self.workerWaiting(worker)
        }
    case MDPW_REPLY:
        if workerReady {
            client := msg[0]
            msg = append([][]byte{client, nil, []byte(MDPC_CLIENT), []byte(worker.service.name)}, msg[2:]...)
            self.socket.SendMultipart(msg, 0)
            self.workerWaiting(worker)
        } else {
            self.deleteWorker(worker, true)
        }
    case MDPW_HEARTBEAT:
        if workerReady {
            worker.expiry = time.Now().Add(HEARTBEAT_EXPIRY)
        } else {
            self.deleteWorker(worker, true)
        }
    case MDPW_DISCONNECT:
        self.deleteWorker(worker, false)
    default:
        log.Println("E: invalid message:")
        Dump(msg)
    }
}

func (self *mdBroker) purgeWorkers() {
    now := time.Now()
    for elem := self.waiting.Front(); elem != nil; elem = self.waiting.Front() {
        worker, _ := elem.Value.(*mdWorker)
        if worker.expiry.After(now) {
            break
        }
        self.deleteWorker(worker, false)
    }
}

func(self *mdBroker) requireService(name string) *mdService {
    if len(name) == 0 { panic("Invalid service name") }
    service, ok := self.services[name];
    if !ok {
        service = &mdService{
            name: name,
            waiting: NewList(),
        }
        self.services[name] = service
    }
    return service
}

func (self *mdBroker) sendToWorker(worker *mdWorker, command string, option []byte, msg [][]byte) {
    if len(option) > 0 {
        msg = append([][]byte{option}, msg...)
    }
    msg = append([][]byte{worker.address, nil, []byte(MDPW_WORKER), []byte(command)}, msg...)

    if self.verbose {
        log.Printf("I: sending %X to worker\n", command)
        Dump(msg)
    }
    self.socket.SendMultipart(msg, 0)
}

func (self *mdBroker) serviceInternal(service []byte, msg [][]byte) {
    returncode := "501"
    if string(service) == "mmi.service" {
        name := string(msg[len(msg)-1])
        if _, ok := self.services[name]; ok {
            returncode = "200"
        } else {
            returncode = "404"
        }
    }
    msg[len(msg)-1] = []byte(returncode)
    msg = append(append(msg[2:], []byte(MDPC_CLIENT), service), msg[3:]...)
    self.socket.SendMultipart(msg, 0)
}

func (self *mdBroker) workerWaiting(worker *mdWorker) {
    self.waiting.PushBack(worker)
    worker.service.waiting.PushBack(worker)
    worker.expiry = time.Now().Add(HEARTBEAT_EXPIRY)
    self.dispatch(worker.service, nil)
}

func (self *mdBroker) Close() {
    if self.socket != nil {
        self.socket.Close()
    }
    self.context.Close()
}

func (self *mdBroker) Run() {
    for {
        items := zmq.PollItems{
            zmq.PollItem{Socket: self.socket, Events: zmq.POLLIN},
        }

        _, err := zmq.Poll(items, HEARTBEAT_INTERVAL.Nanoseconds()/1e3)
        if err != nil {
            panic(err)
        }

        if item := items[0]; item.REvents&zmq.POLLIN != 0 {
            msg, _ := self.socket.RecvMultipart(0)
            if self.verbose {
                log.Printf("I: received message:")
                Dump(msg)
            }

            sender := msg[0]
            header := msg[2]
            msg = msg[3:]

            if string(header) == MDPC_CLIENT {
                self.processClient(sender, msg)
            } else if string(header) == MDPW_WORKER {
                self.processWorker(sender, msg)
            } else {
                log.Println("E: invalid message:")
                Dump(msg)
            }
        }

        if self.heartbeatAt.Before(time.Now()) {
            self.purgeWorkers()
            for elem := self.waiting.Front(); elem != nil; elem = elem.Next() {
                worker, _ := elem.Value.(*mdWorker)
                self.sendToWorker(worker, MDPW_HEARTBEAT, nil, nil)
            }
            self.heartbeatAt = time.Now().Add(HEARTBEAT_INTERVAL)
        }
    }
}


func main() {
    verbose := len(os.Args) >= 2 && os.Args[1] == "-v"
    broker := NewBroker("tcp://*:5555", verbose)
    defer broker.Close()

    broker.Run()
}
