"""

   Simple message queuing broker
   Same as request-reply broker but using QUEUE device
 
   Author: Guillaume Aubert (gaubert) <guillaume(dot)aubert(at)gmail(dot)com>
  
"""


import zmq

def main():
    """ main method """
    
    context = zmq.Context(1)
    
    # Socket facing clients
    frontend = context.socket(zmq.ROUTER)
    frontend.bind("tcp://*:5559")
    
    # Socket facing services
    backend  = context.socket(zmq.DEALER)
    backend.bind("tcp://*:5560")
    
    zmq.device(zmq.QUEUE, frontend, backend)
    
    # We never get here...
    frontend.close()
    backend.close()
    context.term()
    


if __name__ == "__main__":
    main()
