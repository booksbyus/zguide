"""
Majordomo Protocol client example. Uses the mdcli API to hide all MDP aspects

Author : Min RK <benjaminrk@gmail.com>

"""

import sys
from mdcliapi2 import MajorDomoClient

def main():
    verbose = '-v' in sys.argv
    client = MajorDomoClient("tcp://localhost:5555", verbose)
    requests = 100000
    for i in xrange(requests):
        request = "Hello world"
        try:
            client.send("echo", request)
        except KeyboardInterrupt:
            print "send interrupted, aborting"
            return
    
    count = 0
    while count < requests:
        try:
            reply = client.recv()
        except KeyboardInterrupt:
            break
        else:
            # also break on failure to reply:
            if reply is None:
                break
        count += 1
    print "%i requests/replies processed" % count
    
if __name__ == '__main__':
    main()

