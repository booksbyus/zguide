#
#   Broker peering simulation (part 1) in Python
#   Prototypes the state flow
#
#   Author : Piero Cornice
#   Contact: root(at)pieroland(dot)net
#

import zmq
import time
import random

def main(args):

    myself = args[1]
    print "Hello, I am", myself

    context = zmq.Context()

    # State Back-End
    statebe = context.socket(zmq.PUB)

    # State Front-End
    statefe = context.socket(zmq.SUB)
    statefe.setsockopt(zmq.SUBSCRIBE, '')

    bind_address = "ipc://" + myself + "-state.ipc"
    statebe.bind(bind_address)

    for i in range(len(args) - 2):
        endpoint = "ipc://" + args[i + 2] + "-state.ipc"
        statefe.connect(endpoint)
        time.sleep(1.0)

    poller = zmq.Poller()
    poller.register(statefe, zmq.POLLIN)

    while True:

########## Solution with poll() ##########
        socks = dict(poller.poll(1000))

        try:
            # Handle incoming status message
            if socks[statefe] == zmq.POLLIN:
                msg = statefe.recv_multipart()
                print 'Received:', msg

        except KeyError:
            # Send our address and a random value
            # for worker availability
            msg = []
            msg.append(bind_address)
            msg.append(str(random.randrange(1, 10)))
            statebe.send_multipart(msg)
##################################

######### Solution with select() #########
#        (pollin, pollout, pollerr) = zmq.select([statefe], [], [], 1)
#
#        if len(pollin) > 0 and pollin[0] == statefe:
#            # Handle incoming status message
#            msg = statefe.recv_multipart()
#            print 'Received:', msg
#
#        else:
#            # Send our address and a random value
#            # for worker availability
#            msg = []
#            msg.append(bind_address)
#            msg.append(str(random.randrange(1, 10)))
#            statebe.send_multipart(msg)
##################################

    poller.unregister(statefe)
    time.sleep(1.0)


if __name__ == '__main__':
    import sys

    if len(sys.argv) < 2:
        print "Usage: peering.py <myself> <peer_1> ... <peer_N>"
        raise SystemExit

    main(sys.argv)

