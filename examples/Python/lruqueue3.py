"""

   Least-recently used (LRU) queue device
   Demonstrates use of pyzmq IOLoop reactor
   
   While this example runs in a single process, that is just to make
   it easier to start and stop the example. Each thread has its own
   context and conceptually acts as a separate process.
    
   Author: Min RK <benjaminrk(at)gmail(dot)com>
   Adapted from lruqueue.py by Guillaume Aubert (gaubert) <guillaume(dot)aubert(at)gmail(dot)com>
  
"""

import threading
import time
import zmq

from zmq.eventloop.ioloop import IOLoop
from zmq.eventloop.zmqstream import ZMQStream

NBR_CLIENTS = 10
NBR_WORKERS = 3

def worker_thread(worker_url, i):
    """Worker using REQ socket to do LRU routing"""
    context = zmq.Context()
    socket = context.socket(zmq.REQ)

    identity = "Worker-%d" % (i)
    
    socket.setsockopt(zmq.IDENTITY, identity) #set worker identity
    
    socket.connect(worker_url)
    
    # Tell the server we are ready for work
    socket.send("READY")
    
    try:
        while True:
            
            address, empty, request = socket.recv_multipart()
            
            print "%s: %s\n" % (identity, request),
               
            socket.send_multipart([address, '', 'OK'])
            
    except zmq.ZMQError, zerr:
        # context terminated so quit silently
        if zerr.strerror == 'Context was terminated':
            return
        else:
            raise zerr
    
        
def client_thread(client_url, i):
    """Basic request-reply client using REQ socket"""
    context = zmq.Context()
    socket = context.socket(zmq.REQ)

    identity = "Client-%d" % (i)
    
    socket.setsockopt(zmq.IDENTITY, identity) #Set client identity. Makes tracing easier
    
    socket.connect(client_url)

    #  Send request, get reply
    socket.send("HELLO")
    reply = socket.recv()
    
    print "%s: %s\n" % (identity, reply),
    

class LRUQueue(object):
    """LRUQueue class using ZMQStream/IOLoop for event dispatching"""
    
    def __init__(self, backend_socket, frontend_socket):
        self.available_workers = 0
        self.workers = []
        self.client_nbr = NBR_CLIENTS
        
        self.backend = ZMQStream(backend_socket)
        self.frontend = ZMQStream(frontend_socket)
        self.backend.on_recv(self.handle_backend)
        
        self.loop = IOLoop.instance()
    
    def handle_backend(self, msg):
        # Queue worker address for LRU routing
        worker_addr, empty, client_addr = msg[:3]
        
        assert self.available_workers < NBR_WORKERS
        
        # add worker back to the list of workers
        self.available_workers += 1
        self.workers.append(worker_addr)
        
        #   Second frame is empty
        assert empty == ""
        
        # Third frame is READY or else a client reply address
        # If client reply, send rest back to frontend
        if client_addr != "READY":
            empty, reply = msg[3:]
            
            # Following frame is empty
            assert empty == ""
            
            self.frontend.send_multipart([client_addr, '', reply])
            
            self.client_nbr -= 1
            
            if self.client_nbr == 0:
                # Exit after N messages
                self.loop.add_timeout(time.time()+1, self.loop.stop)
        
        if self.available_workers == 1:
            # on first recv, start accepting frontend messages
            self.frontend.on_recv(self.handle_frontend)
    
    def handle_frontend(self, msg):
        # Now get next client request, route to LRU worker
        # Client request is [address][empty][request]
        client_addr, empty, request = msg
        
        assert empty == ""
        
        #  Dequeue and drop the next worker address
        self.available_workers -= 1
        worker_id = self.workers.pop()
         
        self.backend.send_multipart([worker_id, '', client_addr, '', request])
        if self.available_workers == 0:
            # stop receiving until workers become available again
            self.frontend.stop_on_recv()

def main():
    """main method"""

    url_worker = "ipc://backend.ipc"
    url_client = "ipc://frontend.ipc"
    
    # Prepare our context and sockets
    context = zmq.Context()
    frontend = context.socket(zmq.ROUTER)
    frontend.bind(url_client)
    backend = context.socket(zmq.ROUTER)
    backend.bind(url_worker)
    
    # create workers and clients threads
    for i in range(NBR_WORKERS):
        thread = threading.Thread(target=worker_thread, args=(url_worker, i, ))
        thread.daemon = True
        thread.start()
    
    for i in range(NBR_CLIENTS):
        thread_c = threading.Thread(target=client_thread, args=(url_client, i, ))
        thread_c.daemon = True
        thread_c.start()
    
    # create queue with the sockets
    queue = LRUQueue(backend, frontend)
    
    # start reactor
    IOLoop.instance().start()

if __name__ == "__main__":
    main()

