"""
MMI echo query example

Author : Min RK <benjaminrk@gmail.com>

"""

import sys
from mdcliapi import MajorDomoClient

def main():
    verbose = '-v' in sys.argv
    client = MajorDomoClient("tcp://localhost:5555", verbose)
    request = "echo"
    reply = client.send("mmi.service", request)
    
    if reply:
        replycode = reply[0]
        print "Lookup echo service:", replycode
    else:
        print "E: no response from broker, make sure it's running"
    
if __name__ == '__main__':
    main()

