#
# Last value cache
# Uses XPUB subscription messages to re-send data
#

import zmq

def main():
    ctx = zmq.Context.instance()
    frontend = ctx.socket(zmq.SUB)
    frontend.connect("tcp://*:5557")
    backend = ctx.socket(zmq.XPUB)
    backend.bind("tcp://*:5558")

    # Subscribe to every single topic from publisher
    frontend.setsockopt(zmq.SUBSCRIBE, b"")

    # Store last instance of each topic in a cache
    cache = {}

    # main poll loop
    # We route topic updates from frontend to backend, and
    # we handle subscriptions by sending whatever we cached,
    # if anything:
    poller = zmq.Poller()
    poller.register(frontend, zmq.POLLIN)
    poller.register(backend, zmq.POLLIN)
    while True:

        try:
            events = dict(poller.poll(1000))
        except KeyboardInterrupt:
            print("interrupted")
            break

        # Any new topic data we cache and then forward
        if frontend in events:
            msg = frontend.recv_multipart()
            topic, current = msg
            cache[topic] = current
            backend.send_multipart(msg)

        # handle subscriptions
        # When we get a new subscription we pull data from the cache:
        if backend in events:
            event = backend.recv()
            # Event is one byte 0=unsub or 1=sub, followed by topic
            if event[0] == b'\x01':
                topic = event[1:]
                if topic in cache:
                    print ("Sending cached topic %s" % topic)
                    backend.send_multipart([ topic, cache[topic] ])

if __name__ == '__main__':
    main()
