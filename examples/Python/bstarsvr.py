from argparse import ArgumentParser
import time

import zmq

STATE_PRIMARY = 1
STATE_BACKUP = 2
STATE_ACTIVE = 3
STATE_PASSIVE = 4

PEER_PRIMARY = 1
PEER_BACKUP = 2
PEER_ACTIVE = 3
PEER_PASSIVE = 4
CLIENT_REQUEST = 5

HEARTBEAT = 1000

pyzmq_version = tuple(map(int, zmq.pyzmq_version().split('.')))
if pyzmq_version <= (2, 1, 7):
    zmq.ROUTER = zmq.XREP


class BStarState(object):
    def __init__(self, state, event, peer_expiry):
        self.state = state
        self.event = event
        self.peer_expiry = peer_expiry


class BStarException(Exception):
    pass

fsm_states = {
    STATE_PRIMARY: {
        PEER_BACKUP: ("I: connected to backup (slave), ready as master",
                      STATE_ACTIVE),
        PEER_ACTIVE: ("I: connected to backup (master), ready as slave",
                      STATE_PASSIVE)
        },
    STATE_BACKUP: {
        PEER_ACTIVE: ("I: connected to primary (master), ready as slave",
                      STATE_PASSIVE),
        CLIENT_REQUEST: ("", False)
        },
    STATE_ACTIVE: {
        PEER_ACTIVE: ("E: fatal error - dual masters, aborting", False)
        },
    STATE_PASSIVE: {
        PEER_PRIMARY: ("I: primary (slave) is restarting, ready as master",
                       STATE_ACTIVE),
        PEER_BACKUP: ("I: backup (slave) is restarting, ready as master",
                      STATE_ACTIVE),
        PEER_PASSIVE: ("E: fatal error - dual slaves, aborting", False),
        CLIENT_REQUEST: (CLIENT_REQUEST, True)
        }
    }


def run_fsm(fsm):
    # There are some transitional states we do not want to handle
    res = fsm_states[fsm.state].get(fsm.event)
    if res:
        msg, state = res
    else:
        return
    if state == False:
        raise BStarException(msg)
    elif msg == CLIENT_REQUEST:
        assert fsm.peer_expiry > 0
        if int(time.time()) > fsm.peer_expiry:
            fsm.state = STATE_ACTIVE
        else:
            raise BStarException()
    else:
        print msg
        fsm.state = state


def main():
    parser = ArgumentParser()
    parser.add_argument("-p", "--primary", action="store_true", default=False)
    parser.add_argument("-b", "--backup", action="store_true", default=False)
    args = parser.parse_args()
    if args.primary and args.backup:
        args.print_help()
        exit(-1)

    ctx = zmq.Context()
    statepub = ctx.socket(zmq.PUB)
    statesub = ctx.socket(zmq.SUB)
    statesub.setsockopt(zmq.SUBSCRIBE, "")
    frontend = ctx.socket(zmq.ROUTER)

    fsm = BStarState(0, 0, 0)

    if args.primary:
        print "I: Primary master, waiting for backup (slave)"
        frontend.bind("tcp://*:5001")
        statepub.bind("tcp://*:5003")
        statesub.connect("tcp://localhost:5004")
        fsm.state = STATE_PRIMARY
    elif args.backup:
        print "I: Backup slave, waiting for primary (master)"
        frontend.bind("tcp://*:5002")
        statepub.bind("tcp://*:5004")
        statesub.connect("tcp://localhost:5003")
        statesub.setsockopt(zmq.SUBSCRIBE, "")
        fsm.state = STATE_BACKUP

    send_state_at = int(time.time() * 1000 + HEARTBEAT)
    poller = zmq.Poller()
    poller.register(frontend, zmq.POLLIN)
    poller.register(statesub, zmq.POLLIN)

    while True:
        time_left = send_state_at - int(time.time() * 1000)
        if time_left < 0:
            time_left = 0
        socks = dict(poller.poll(time_left))
        if socks.get(frontend) == zmq.POLLIN:
            msg = frontend.recv()
            fsm.state = CLIENT_REQUEST
            try:
                run_fsm(fsm)
                frontend.send(msg)
            except BStarException:
                del msg

        if socks.get(statesub) == zmq.POLLIN:
            msg = statesub.recv()
            fsm.event = int(msg)
            del msg
            try:
                run_fsm(fsm)
                fsm.peer_expiry = int(time.time() * 1000 + 2) * HEARTBEAT
            except BStarException:
                break
        if int(time.time() * 1000) >= send_state_at:
            statepub.send("%d" % fsm.state)
            send_state_at = int(time.time() * 1000 + HEARTBEAT)

if __name__ == '__main__':
    main()
