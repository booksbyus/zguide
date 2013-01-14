//  UDP ping command
//  Model 1, does UDP work inline

#include <czmq.h>
#define PING_PORT_NUMBER 9999
#define PING_MSG_SIZE    1
#define PING_INTERVAL    1000  //  Once per second

static void
derp (char *s)
{
    perror (s);
    exit (1);
}

int main (void)
{
    zctx_t *ctx = zctx_new ();

    //  Create UDP socket
    int fd;
    if ((fd = socket (AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1)
        derp ("socket");

    //  Ask operating system to let us do broadcasts from socket
    int on = 1;
    if (setsockopt (fd, SOL_SOCKET, SO_BROADCAST, &on, sizeof (on)) == -1)
        derp ("setsockopt (SO_BROADCAST)");

    //  Bind UDP socket to local port so we can receive pings
    struct sockaddr_in si_this = { 0 };
    si_this.sin_family = AF_INET;
    si_this.sin_port = htons (PING_PORT_NUMBER);
    si_this.sin_addr.s_addr = htonl (INADDR_ANY);
    if (bind (fd, &si_this, sizeof (si_this)) == -1)
        derp ("bind");
    
    byte buffer [PING_MSG_SIZE];
    
    //  .split main ping loop
    //  We use {{zmq_poll}} to wait for activity on the UDP socket, because
    //  this function works on non-0MQ file handles. We send a beacon
    //  once a second, and we collect and report beacons that come in
    //  from other nodes:
    
    zmq_pollitem_t pollitems [] = {{ NULL, fd, ZMQ_POLLIN, 0 }};
    //  Send first ping right away
    uint64_t ping_at = zclock_time ();
    
    while (!zctx_interrupted) {
        long timeout = (long) (ping_at - zclock_time ());
        if (timeout < 0)
            timeout = 0;
        if (zmq_poll (pollitems, 1, timeout * ZMQ_POLL_MSEC) == -1)
            break;              //  Interrupted

        //  Someone answered our ping
        if (pollitems [0].revents & ZMQ_POLLIN) {
            struct sockaddr_in si_that;
            socklen_t si_len;
            ssize_t size = recvfrom (fd, buffer, PING_MSG_SIZE, 0,
                                     &si_that, &si_len);
            if (size == -1)
                derp ("recvfrom");
            printf ("Found peer %s:%d\n",
                inet_ntoa (si_that.sin_addr), ntohs (si_that.sin_port));
        }
        if (zclock_time () >= ping_at) {
            //  Broadcast our beacon
            puts ("Pinging peers...");
            buffer [0] = '!';
            struct sockaddr_in si_that = si_this;
            inet_aton ("255.255.255.255", &si_that.sin_addr);
            if (sendto (fd, buffer, PING_MSG_SIZE, 0,
                        &si_that, sizeof (struct sockaddr_in)) == -1)
                derp ("sendto");
            ping_at = zclock_time () + PING_INTERVAL;
        }
    }
    close (fd);
    zctx_destroy (&ctx);
    return 0;
}
