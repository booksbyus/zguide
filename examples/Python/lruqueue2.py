"""

   Least-recently used (LRU) queue device
   Clients and workers are shown here in-process
 
   Author: Guillaume Aubert (gaubert) <guillaume(dot)aubert(at)gmail(dot)com>
  
"""

import threading
import time
import zmq

NBR_CLIENTS = 10
NBR_WORKERS = 3

def worker_thread(worker_url, context, i):
    """ Worker using REQ socket to do LRU routing """
    
    socket = context.socket(zmq.REQ)

    identity = "Worker-%d" % (i)
    
    socket.setsockopt(zmq.IDENTITY, identity) #set worker identity
        
    socket.connect(worker_url)
    
    # Tell the borker we are ready for work
    socket.send("READY")
    
    try:
        while True:
            
            [address, empty, request] = socket.recv_multipart()
            
            print("%s: %s\n" %(identity, request))
               
            socket.send_multipart([address, "", "OK"])
            
    except zmq.ZMQError, zerr:
        # context terminated so quit silently
        if zerr.strerror == 'Context was terminated':
            return
        else:
            raise zerr
    
        
def client_thread(client_url, context, i):
    """ Basic request-reply client using REQ socket """
    
    socket = context.socket(zmq.REQ)

    identity = "Client-%d" % (i)
    
    socket.setsockopt(zmq.IDENTITY, identity) #Set client identity. Makes tracing easier
    
    socket.connect(client_url)

    #  Send request, get reply
    socket.send("HELLO")
    
    reply = socket.recv()
    
    print("%s: %s\n" % (identity, reply))
    
    return
    
        
def main():
    """ main method """

    url_worker = "inproc://workers"
    url_client = "inproc://clients"
    client_nbr = NBR_CLIENTS
    
    # Prepare our context and sockets
    context = zmq.Context(1)
    frontend = context.socket(zmq.XREP)
    frontend.bind(url_client)
    backend = context.socket(zmq.XREP)
    backend.bind(url_worker)
    
    
    
    # create workers and clients threads
    for i in range(NBR_WORKERS):
        thread = threading.Thread(target=worker_thread, args=(url_worker, context, i, ))
        thread.start()
    
    for i in range(NBR_CLIENTS):
        thread_c = threading.Thread(target=client_thread, args=(url_client, context, i, ))
        thread_c.start()
    
    # Logic of LRU loop
    # - Poll backend always, frontend only if 1+ worker ready
    # - If worker replies, queue worker as ready and forward reply
    # to client if necessary
    # - If client requests, pop next worker and send request to it
    
    # Queue of available workers
    available_workers = 0
    workers_list      = []
    
    # init poller
    poller = zmq.Poller()
    
    # Always poll for worker activity on backend
    poller.register(backend, zmq.POLLIN)
    
    # Poll front-end only if we have available workers
    poller.register(frontend, zmq.POLLIN)
    
    while True:
        
        socks = dict(poller.poll())
    
        # Handle worker activity on backend
        if (backend in socks and socks[backend] == zmq.POLLIN):
            
            # Queue worker address for LRU routing
            message = backend.recv_multipart()
        
            assert available_workers < NBR_WORKERS
            
            worker_addr = message[0]
            
            # add worker back to the list of workers
            available_workers += 1
            workers_list.append(worker_addr)
            
            #   Second frame is empty
            empty        = message[1]
            assert empty == ""
            
            # Third frame is READY or else a client reply address
            client_addr = message[2]
            
            # If client reply, send rest back to frontend
            if client_addr != "READY":
                
                # Following frame is empty
                empty = message[3]
                assert empty == ""
                
                reply = message[4]
                  
                frontend.send_multipart([client_addr, "", reply])
                
                client_nbr -= 1
                
                if client_nbr == 0:
                    break  # Exit after N messages
    
        # poll on frontend only if workers are available
        if available_workers > 0:
            
            if (frontend in socks and socks[frontend] == zmq.POLLIN):
                # Now get next client request, route to LRU worker
                # Client request is [address][empty][request]
                
                [client_addr, empty, request ] = frontend.recv_multipart()
                
                assert empty == ""
                
                #  Dequeue and drop the next worker address
                available_workers -= 1
                worker_id = workers_list.pop()
                
                backend.send_multipart([worker_id, "", client_addr, "", request])
        

    #out of infinite loop: do some housekeeping
    time.sleep (1)
    
    frontend.close()
    backend.close()
    context.term()
    

if __name__ == "__main__":
    main()

