from time import sleep
import zmq


REQUEST_TIMEOUT = 1000  # msecs
SETTLE_DELAY = 2000  # before failing over


def main():
    server = ['tcp://localhost:5001', 'tcp://localhost:5002']
    server_nbr = 0
    ctx = zmq.Context()
    client = ctx.socket(zmq.REQ)
    client.connect(server[server_nbr])
    poller = zmq.Poller()
    poller.register(client, zmq.POLLIN)

    sequence = 0
    while True:
        client.send("%s" % sequence)

        expect_reply = True
        while expect_reply:
            socks = dict(poller.poll(REQUEST_TIMEOUT))
            if socks.get(client) == zmq.POLLIN:
                reply = client.recv()
                if int(reply) == sequence:
                    print "I: server replied OK (%s)" % reply
                    expect_reply = False
                    sequence += 1
                    sleep(1)
                else:
                    print "E: malformed reply from server: %s" % reply
            else:
                print "W: no response from server, failing over"
                sleep(SETTLE_DELAY / 1000)
                poller.unregister(client)
                client.close()
                server_nbr = (server_nbr + 1) % 2
                print "I: connecting to server at %s.." % server[server_nbr]
                client = ctx.socket(zmq.REQ)
                poller.register(client, zmq.POLLIN)
                # reconnect and resend request
                client.connect(server[server_nbr])
                client.send("%s" % sequence)

if __name__ == '__main__':
    main()
