#
#   Broker peering simulation (part 1) in Python
#   Prototypes the state flow
#
#   Author : Piero Cornice
#   Contact: root(at)pieroland(dot)net
#
import sys
import time
import random

import zmq


def main(myself, others):
    print "Hello, I am", myself

    context = zmq.Context()

    # State Back-End
    statebe = context.socket(zmq.PUB)

    # State Front-End
    statefe = context.socket(zmq.SUB)
    statefe.setsockopt(zmq.SUBSCRIBE, '')

    bind_address = "ipc://%s-state.ipc" % myself
    statebe.bind(bind_address)

    for other in others:
        statefe.connect("ipc://%s-state.ipc" % other)
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
            msg = [bind_address, str(random.randrange(1, 10))]
            statebe.send_multipart(msg)
##################################

######### Solution with select() #########
#        pollin, pollout, pollerr = zmq.select([statefe], [], [], 1)
#
#        if pollin and pollin[0] == statefe:
#            # Handle incoming status message
#            msg = statefe.recv_multipart()
#            print 'Received:', msg
#
#        else:
#            # Send our address and a random value
#            # for worker availability
#            msg = [bind_address, str(random.randrange(1, 10))]
#            statebe.send_multipart(msg)
##################################

    poller.unregister(statefe)
    time.sleep(1.0)


if __name__ == '__main__':
    if len(sys.argv) >= 2:
        main(myself=sys.argv[1], others=sys.argv[2:])
    else:
        print "Usage: peering.py <myself> <peer_1> ... <peer_N>"
        sys.exit(1)
