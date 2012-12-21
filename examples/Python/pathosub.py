#
# Pathological subscriber
# Subscribes to one random topic and prints received messages
#

import sys
import time

from random import randint

import zmq

def main(url=None):
    ctx = zmq.Context.instance()
    subscriber = ctx.socket(zmq.SUB)
    if url is None:
        url = "tcp://localhost:5556"
    subscriber.connect(url)

    subscription = b"%03d" % randint(0,999)
    subscriber.setsockopt(zmq.SUBSCRIBE, subscription)

    while True:
        topic, data = subscriber.recv_multipart()
        assert topic == subscription
        print data

if __name__ == '__main__':
    main(sys.argv[1] if len(sys.argv) > 1 else None)
