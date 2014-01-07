"""

   Pubsub envelope publisher

   Author: Guillaume Aubert (gaubert) <guillaume(dot)aubert(at)gmail(dot)com>

"""
import time
import zmq

def main():
    """main method"""

    # Prepare our context and publisher
    context   = zmq.Context()
    publisher = context.socket(zmq.PUB)
    publisher.bind("tcp://*:5563")

    while True:
        # Write two messages, each with an envelope and content
        publisher.send_multipart([b"A", b"We don't want to see this"])
        publisher.send_multipart([b"B", b"We would like to see this"])
        time.sleep(1)

    # We never get here but clean up anyhow
    publisher.close()
    context.term()


if __name__ == "__main__":
    main()
