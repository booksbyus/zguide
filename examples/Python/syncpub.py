#
#  Synchronized publisher
#
import zmq

#  We wait for 10 subscribers
SUBSCRIBERS_EXPECTED = 10

def main():
    context = zmq.Context()
    
    # Socket to talk to clients
    publisher = context.socket(zmq.PUB)
    publisher.bind('tcp://*:5561')

    # Socket to receive signals
    syncservice = context.socket(zmq.REP)
    syncservice.bind('tcp://*:5562')

    # Get synchronization from subscribers
    subscribers = 0
    while subscribers < SUBSCRIBERS_EXPECTED:
        # wait for synchronization request
        msg = syncservice.recv()
        # send synchronization reply
        syncservice.send('')
        subscribers += 1
        print "+1 subscriber"
    
    # Now broadcast exactly 1M updates followed by END
    for i in range(1000000):
       publisher.send('Rhubarb');

    publisher.send('END')

if __name__ == '__main__':
    main()