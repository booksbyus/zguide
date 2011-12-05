"""
Clone server Model Six

"""

import random
import time

import zmq

from clone import Clone

SUBTREE = "/client/"

def main():
    # Create and connect clone
    clone = Clone()
    clone.subtree = SUBTREE
    clone.connect("tcp://localhost", 5556)
    clone.connect("tcp://localhost", 5566)
    
    try:
        while True:
            # Distribute as key-value message
            key = "%d" % random.randint(1,10000)
            value = "%d" % random.randint(1,1000000)
            clone.set(key, value, random.randint(0,30))
            time.sleep(1)
    except KeyboardInterrupt:
        pass

if __name__ == '__main__':
    main()