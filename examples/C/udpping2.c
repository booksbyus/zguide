//  UDP ping command
//  Model 2, uses separate UDP library

#include <czmq.h>
#include "udplib.c"
#define PING_PORT_NUMBER 9999
#define PING_MSG_SIZE    1
#define PING_INTERVAL    1000  //  Once per second

int main (void)
{
    zctx_t *ctx = zctx_new ();
    udp_t *udp = udp_new (PING_PORT_NUMBER);
    
    byte buffer [PING_MSG_SIZE];
    zmq_pollitem_t pollitems [] = {
        { NULL, udp_handle (udp), ZMQ_POLLIN, 0 }
    };
    //  Send first ping right away
    uint64_t ping_at = zclock_time ();
    
    while (!zctx_interrupted) {
        long timeout = (long) (ping_at - zclock_time ());
        if (timeout < 0)
            timeout = 0;
        if (zmq_poll (pollitems, 1, timeout * ZMQ_POLL_MSEC) == -1)
            break;              //  Interrupted

        //  Someone answered our ping
        if (pollitems [0].revents & ZMQ_POLLIN)
            udp_recv (udp, buffer, PING_MSG_SIZE);
        
        if (zclock_time () >= ping_at) {
            puts ("Pinging peers...");
            buffer [0] = '!';
            udp_send (udp, buffer, PING_MSG_SIZE);
            ping_at = zclock_time () + PING_INTERVAL;
        }
    }
    udp_destroy (&udp);
    zctx_destroy (&ctx);
    return 0;
}
