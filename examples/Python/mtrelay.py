"""

   Multithreaded relay
 
   Author: Guillaume Aubert (gaubert) <guillaume(dot)aubert(at)gmail(dot)com>
  
"""

import threading
import zmq

def step1(context):
    """ step1 """
    
    # Signal downstream to step 2
    sender = context.socket(zmq.PAIR)
    sender.connect("inproc://step2")
    
    sender.send("")
    


def step2(context):
    """ step2 """
    
    # Bind to inproc: endpoint, then start upstream thread
    receiver = context.socket(zmq.PAIR)
    receiver.bind("inproc://step2")
    
    thread = threading.Thread(target=step1, args=(context, ))
    thread.start()
    
    # Wait for signal
    string = receiver.recv()

    # Signal downstream to step 3
    sender = context.socket(zmq.PAIR)
    sender.connect("inproc://step3")
    sender.send("")
    
    return

def main():
    """ server routine """ 
    # Prepare our context and sockets
    context = zmq.Context(1)
    
    # Bind to inproc: endpoint, then start upstream thread
    receiver = context.socket(zmq.PAIR)
    receiver.bind("inproc://step3")
    
    thread = threading.Thread(target=step2, args=(context, ))
    thread.start()
    
    # Wait for signal 
    string = receiver.recv()
    
    print("Test successful!\n")
    
    receiver.close()
    context.term()
    
    return


if __name__ == "__main__":
    main()
