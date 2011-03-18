<?php

/*
 * Round-trip demonstrator
 * 
 * While this example runs in a single process, that is just to make
 * it easier to start and stop the example. Each thread has its own
 * context and conceptually acts as a separate process.
 */
include "zmsg.php";

public function client_task() {
	$context = new ZMQContext();
	$client = new ZMQSocket($context, ZMQ::SOCKET_XREQ);
	$client->setSockOpt(ZMQ::SOCKOPT_IDENTITY, "C");
	$client->connect("tcp://localhost:5555");
	
	echo "Setting up test...", PHP_EOL;
	usleep(10000);
	
	echo "Synchronous round-trip test...", PHP_EOL;
	$start = microtime(true);
	for($requests = 0; $requests < 10000; $requests++) {
		$client->send("HELLO");
		$msg = $client->recv();
	}
}

    printf (" %d calls/second\n",
        (1000 * 10000) / (int) (s_clock () - start));

    printf ("Asynchronous round-trip test...\n");
    start = s_clock ();
    for (requests = 0; requests < 100000; requests++) {
        zmsg_t *msg = zmsg_new ("HELLO");
        zmsg_send (&msg, client);
    }
    for (requests = 0; requests < 100000; requests++) {
        zmsg_t *msg = zmsg_recv (client);
        zmsg_destroy (&msg);
    }
    printf (" %d calls/second\n",
        (1000 * 100000) / (int) (s_clock () - start));

    zmq_close (client);
    zmq_term (context);
    return NULL;
}

static void *
worker_task (void *args)
{
    void *context = zmq_init (1);
    void *worker = zmq_socket (context, ZMQ_XREQ);
    zmq_setsockopt (worker, ZMQ_IDENTITY, "W", 1);
    zmq_connect (worker, "tcp://localhost:5556");

    while (1) {
        zmsg_t *msg = zmsg_recv (worker);
        zmsg_send (&msg, worker);
    }
    zmq_close (worker);
    zmq_term (context);
    return NULL;
}

static void *
broker_task (void *args)
{
    //  Prepare our context and sockets
    void *context = zmq_init (1);
    void *frontend = zmq_socket (context, ZMQ_XREP);
    void *backend  = zmq_socket (context, ZMQ_XREP);
    zmq_bind (frontend, "tcp://*:5555");
    zmq_bind (backend,  "tcp://*:5556");

    //  Initialize poll set
    zmq_pollitem_t items [] = {
        { frontend, 0, ZMQ_POLLIN, 0 },
        { backend,  0, ZMQ_POLLIN, 0 }
    };
    while (1) {
        zmq_poll (items, 2, -1);
        if (items [0].revents & ZMQ_POLLIN) {
            zmsg_t *msg = zmsg_recv (frontend);
            free (zmsg_pop (msg));
            zmsg_push (msg, "W");
            zmsg_send (&msg, backend);
        }
        if (items [1].revents & ZMQ_POLLIN) {
            zmsg_t *msg = zmsg_recv (backend);
            free (zmsg_pop (msg));
            zmsg_push (msg, "C");
            zmsg_send (&msg, frontend);
        }
    }
    zmq_close (frontend);
    zmq_close (backend);
    zmq_term (context);
    return NULL;
}

int main (void)
{
    s_version_assert (2, 1);

    pthread_t client;
    pthread_create (&client, NULL, client_task, NULL);
    pthread_t worker;
    pthread_create (&worker, NULL, worker_task, NULL);
    pthread_t broker;
    pthread_create (&broker, NULL, broker_task, NULL);
    pthread_join (client, NULL);
    return 0;
}
