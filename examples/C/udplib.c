#include <czmq.h>

//  A UDP class.
//  Implemented as a proper wee class using the CZMQ style.
//  Include directly into your application source code.

//  -----------------------------------------------------------------
//  UDP instance

typedef struct {
    int handle;                 //  Socket for send/recv
    int port;                   //  UDP port we work on
    //  Own address
    struct sockaddr_in address;
    //  Broadcast address
    struct sockaddr_in broadcast;
} udp_t;

//  Handle error from I/O operation

static void
s_handle_io_error (char *reason)
{
#   ifdef __WINDOWS__
    switch (WSAGetLastError ()) {
        case WSAEINTR:        errno = EINTR;      break;
        case WSAEBADF:        errno = EBADF;      break;
        case WSAEWOULDBLOCK:  errno = EAGAIN;     break;
        case WSAEINPROGRESS:  errno = EAGAIN;     break;
        case WSAENETDOWN:     errno = ENETDOWN;   break;
        case WSAECONNRESET:   errno = ECONNRESET; break;
        case WSAECONNABORTED: errno = EPIPE;      break;
        case WSAESHUTDOWN:    errno = ECONNRESET; break;
        case WSAEINVAL:       errno = EPIPE;      break;
        default:              errno = GetLastError ();
    }
#   endif
    if (errno == EAGAIN
    ||  errno == ENETDOWN
    ||  errno == EPROTO
    ||  errno == ENOPROTOOPT
    ||  errno == EHOSTDOWN
#   ifdef ENONET // Linux-specific
    ||  errno == ENONET
#   endif
    ||  errno == EHOSTUNREACH
    ||  errno == EOPNOTSUPP
    ||  errno == ENETUNREACH
    ||  errno == EWOULDBLOCK
    ||  errno == EINTR)
        return;             //  Ignore error and try again
    else
    if (errno == EPIPE
    ||  errno == ECONNRESET)
        return;             //  Ignore error and try again
    else {
        zclock_log ("E: (udp) error '%s' on %s", strerror (errno), reason);
        assert (false);
    }
}


//  -----------------------------------------------------------------
//  Constructor

static udp_t *
udp_new (int port)
{
    udp_t *self = (udp_t *) zmalloc (sizeof (udp_t));
    self->port = port;

    //  Create UDP socket
    self->handle = socket (AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if (self->handle == -1)
        s_handle_io_error ("socket");

    //  Ask operating system to let us do broadcasts from socket
    int on = 1;
    if (setsockopt (self->handle, SOL_SOCKET,
                    SO_BROADCAST, &on, sizeof (on)) == -1)
        s_handle_io_error ("setsockopt (SO_BROADCAST)");

    //  Bind UDP socket to local port so we can receive pings
    struct sockaddr_in sockaddr = { 0 };
    sockaddr.sin_family = AF_INET;
    sockaddr.sin_port = htons (self->port);
    sockaddr.sin_addr.s_addr = htonl (INADDR_ANY);
    if (bind (self->handle, &sockaddr, sizeof (sockaddr)) == -1)
        s_handle_io_error ("bind");

#   if defined (__UNIX__)
    struct ifaddrs *interfaces;
    if (getifaddrs (&interfaces) == 0) {
        struct ifaddrs *interface = interfaces;
        while (interface) {
            //  Hopefully the last interface will be WiFi
            if (interface->ifa_addr->sa_family == AF_INET) {
                self->address = *(struct sockaddr_in *) interface->ifa_addr;
                self->broadcast = *(struct sockaddr_in *) interface->ifa_broadaddr;
                self->broadcast.sin_port = htons (self->port);
            }
            interface = interface->ifa_next;
        }
    }
    freeifaddrs (interfaces);
#   else
#       error "Interface detection TBD on this operating system"
#   endif
    return self;
}


//  -----------------------------------------------------------------
//  Destructor

static void
udp_destroy (udp_t **self_p)
{
    assert (self_p);
    if (*self_p) {
        udp_t *self = *self_p;
        close (self->handle);
        free (self);
        *self_p = NULL;
    }
}


//  Returns UDP socket handle

static int
udp_handle (udp_t *self)
{
    assert (self);
    return self->handle;
}

//  Send message using UDP broadcast

static void
udp_send (udp_t *self, byte *buffer, size_t length)
{
    inet_aton ("255.255.255.255", &self->broadcast.sin_addr);
    if (sendto (self->handle, buffer, length, 0,
                &self->broadcast, sizeof (struct sockaddr_in)) == -1)
        s_handle_io_error ("sendto");
}


//  Receive message from UDP broadcast
//  Returns size of received message, or -1

static ssize_t
udp_recv (udp_t *self, byte *buffer, size_t length)
{
    struct sockaddr_in sockaddr;
    socklen_t si_len = sizeof (struct sockaddr_in);
    
    ssize_t size = recvfrom (self->handle, buffer, length, 0, &sockaddr, &si_len);
    if (size == -1)
        s_handle_io_error ("recvfrom");
    else
    if (sockaddr.sin_addr.s_addr != self->address.sin_addr.s_addr)
        printf ("Found peer %s:%d\n",
            inet_ntoa (sockaddr.sin_addr), ntohs (sockaddr.sin_port));

    return 0;
}
