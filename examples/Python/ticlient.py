"""
Titanic client example
Implements client side of http:rfc.zeromq.org/spec:9

Author : Min RK <benjaminrk@gmail.com>
"""

import sys
import time

from mdcliapi import MajorDomoClient

def service_call (session, service, request):
    """Calls a TSP service

    Returns reponse if successful (status code 200 OK), else None
    """
    reply = session.send(service, request)
    if reply:
        status = reply.pop(0)
        if status == "200":
            return reply
        elif status == "400":
            print "E: client fatal error, aborting"
            sys.exit (1)
        elif status == "500":
            print "E: server fatal error, aborting"
            sys.exit (1)
    else:
        sys.exit (0);    #  Interrupted or failed

def main():
    verbose = '-v' in sys.argv
    session = MajorDomoClient("tcp://localhost:5555", verbose)

    #  1. Send 'echo' request to Titanic
    request = ["echo", "Hello world"]
    reply = service_call(session, "titanic.request", request)

    uuid = None

    if reply:
        uuid = reply.pop(0)
        print "I: request UUID ", uuid

    #  2. Wait until we get a reply
    while True:
        time.sleep (.1)
        request = [uuid]
        reply = service_call (session, "titanic.reply", request)

        if reply:
            reply_string = reply[-1]
            print "Reply:", reply_string

            #  3. Close request
            request = [uuid]
            reply = service_call (session, "titanic.close", request)
            break
        else:
            print "I: no reply yet, trying again..."
            time.sleep(5)     #  Try again in 5 seconds
    return 0

if __name__ == '__main__':
    main()
