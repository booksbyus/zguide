"""Shows how to provoke EAGAIN when reaching HWM"""

import zmq

def main():
    ctx = zmq.Context.instance()
    mailbox = ctx.socket(zmq.DEALER)
    mailbox.sndhwm = 4
    mailbox.sndtimeo = 0
    mailbox.connect("tcp://localhost:9876")
    
    for count in range(10):
        print("Sending message %i" % count)
        try:
            mailbox.send(b"message %i" % count)
        except zmq.Again as e:
            print(e)
            break
    
    ctx.destroy(linger=0)

if __name__ == '__main__':
    main()
