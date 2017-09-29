//  Shows how to handle Ctrl-C

#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <fcntl.h>

#include <zmq.h>

//  Signal handling
//
//  Create a self-pipe and call s_catch_signals(pipe's writefd) in your application
//  at startup, and then exit your main loop if your pipe contains any data.
//  Works especially well with zmq_poll.

#define S_NOTIFY_MSG " "
#define S_ERROR_MSG "Error while writing to self-pipe.\n"
static int s_fd;
static void s_signal_handler (int signal_value)
{
    int rc = write (s_fd, S_NOTIFY_MSG, sizeof(S_NOTIFY_MSG));
    if (rc != sizeof(S_NOTIFY_MSG)) {
        write (STDOUT_FILENO, S_ERROR_MSG, sizeof(S_ERROR_MSG)-1);
        exit(1);
    }
}

static void s_catch_signals (int fd)
{
    s_fd = fd;

    struct sigaction action;
    action.sa_handler = s_signal_handler;
    //  Doesn't matter if SA_RESTART set because self-pipe will wake up zmq_poll
    //  But setting to 0 will allow zmq_read to be interrupted.
    action.sa_flags = 0;
    sigemptyset (&action.sa_mask);
    sigaction (SIGINT, &action, NULL);
    sigaction (SIGTERM, &action, NULL);
}

int main (void)
{
    int rc;

    void *context = zmq_ctx_new ();
    void *socket = zmq_socket (context, ZMQ_REP);
    zmq_bind (socket, "tcp://*:5555");

    int pipefds[2];
    rc = pipe(pipefds);
    if (rc != 0) {
        perror("Creating self-pipe");
        exit(1);
    }
    int flags = fcntl(pipefds[0], F_GETFL, 0);
    if (flags < 0) {
        perror ("fcntl(F_GETFL)");
        exit(1);
    }
    rc = fcntl (pipefds[0], F_SETFL, flags | O_NONBLOCK);
    if (rc != 0) {
        perror ("fcntl(F_SETFL)");
        exit(1);
    }

    s_catch_signals (pipefds[1]);

    zmq_pollitem_t items [] = {
        { 0, pipefds[0], ZMQ_POLLIN, 0 },
        { socket, 0, ZMQ_POLLIN, 0 }
    };

    while (1) {
        rc = zmq_poll (items, 2, -1);
        if (rc == 0) {
            continue;
        } else if (rc < 0) {
            if (errno == EINTR) { continue; }
            perror("zmq_poll");
            exit(1);
        }

        // Signal pipe FD
        if (items [0].revents & ZMQ_POLLIN) {
            char buffer [1];
            read (pipefds[0], buffer, 1);  // clear notifying byte
            printf ("W: interrupt received, killing server...\n");
            break;
        }

        // Read socket
        if (items [1].revents & ZMQ_POLLIN) {
            char buffer [255];
            // Use non-blocking so we can continue to check self-pipe via zmq_poll
            rc = zmq_recv (socket, buffer, 255, ZMQ_DONTWAIT);
            if (rc < 0) {
                if (errno == EAGAIN) { continue; }
                if (errno == EINTR) { continue; }
                perror("recv");
                exit(1);
            }
            printf ("W: recv\n");

            // Now send message back.
            // ...
        }
    }

    printf ("W: cleaning up\n");
    zmq_close (socket);
    zmq_ctx_destroy (context);
    return 0;
}
