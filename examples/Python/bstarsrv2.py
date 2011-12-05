"""
Binary Star server, using bstar reactor

Author: Min RK <benjaminrk@gmail.com>
"""

import sys

import zmq

from bstar import BinaryStar

def echo(socket, msg):
    """Echo service"""
    socket.send_multipart(msg)

def main():
    # Arguments can be either of:
    #     -p  primary server, at tcp://localhost:5001
    #     -b  backup server, at tcp://localhost:5002
    if '-p' in sys.argv:
        star = BinaryStar(True, "tcp://*:5003", "tcp://localhost:5004")
        star.register_voter("tcp://*:5001", zmq.ROUTER, echo)
    elif '-b' in sys.argv:
        star = BinaryStar(False, "tcp://*:5004", "tcp://localhost:5003")
        star.register_voter("tcp://*:5002", zmq.ROUTER, echo)
    else:
        print ("Usage: bstarsrv2.py { -p | -b }\n")
        return
    
    star.start()

if __name__ == '__main__':
    main()