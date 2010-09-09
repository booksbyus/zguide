#
#  Synchronized subscriber
#
import zmq

def main():
    context = zmq.Context()
    
    # First, connect our subscriber socket
    subscriber = context.socket(zmq.SUB)
    subscriber.connect('tcp://localhost:5561')
    subscriber.setsockopt(zmq.SUBSCRIBE, "")

    # Second, synchronize with publisher
    syncclient = context.socket(zmq.REQ)
    syncclient.connect('tcp://localhost:5562')
    
    # send a synchronization request
    syncclient.send('')
    
    # wait for synchronization reply
    syncclient.recv()

    # Third, get our updates and report how many we got
    nbr = 0
    while True:
        msg = subscriber.recv()
        if msg == 'END':
            break
        nbr += 1
    
    print 'Received %d updates' % nbr

if __name__ == '__main__':
    main()