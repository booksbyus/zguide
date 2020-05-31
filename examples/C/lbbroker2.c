//  Load-balancing broker
//  Demonstrates use of the CZMQ API

#include "czmq.h"

#define NBR_CLIENTS 10
#define NBR_WORKERS 3
#define WORKER_READY   "READY"      //  Signals worker is ready

//  Basic request-reply client using REQ socket
//
static void
client_task(zsock_t *pipe, void *args)
{
	// Signal caller zactor has started
	zsock_signal(pipe, 0);
	zsock_t *client = zsock_new(ZMQ_REQ);

#if (defined (WIN32))
	zsock_connect(client, "tcp://localhost:5672"); // frontend
#else
	zsock_connect(client, "ipc://frontend.ipc");
#endif

	//  Send request, get reply
	zstr_send(client, "HELLO");
	char *reply = zstr_recv(client);
	if (reply) {
		printf("Client: %s\n", reply);
		free(reply);
	}

	zsock_destroy(&client);
}

//  Worker using REQ socket to do load-balancing
//
static void
worker_task(zsock_t *pipe, void *args)
{
	// Signal caller zactor has started
	zsock_signal(pipe, 0);
	zsock_t *worker = zsock_new(ZMQ_REQ);

#if (defined (WIN32))
	zsock_connect(worker, "tcp://localhost:5673"); // backend
#else
	zsock_connect(worker, "ipc://backend.ipc");
#endif

    //  Tell broker we're ready for work
	zframe_t *frame = zframe_new(WORKER_READY, strlen(WORKER_READY));
	zframe_send(&frame, worker, 0);

	//  Process messages as they arrive
	zpoller_t *poll = zpoller_new(pipe, worker, NULL);
	while (true) {
		zsock_t *ready = zpoller_wait(poll, -1);
		if (ready == pipe)
            break;              //  Done

		assert(ready == worker);
		zmsg_t *msg = zmsg_recv(worker);
		if (!msg)
			break;              //  Interrupted
		zframe_print(zmsg_last(msg), "Worker: ");
		zframe_reset(zmsg_last(msg), "OK", 2);
		zmsg_send(&msg, worker);
	}

	if (frame)
		zframe_destroy(&frame);
	zsock_destroy(&worker);
	zpoller_destroy(&poll);

	// Signal done
	zsock_signal(pipe, 0);
}

//  .split main task
//  Now we come to the main task. This has the identical functionality to
//  the previous {{lbbroker}} broker example, but uses CZMQ to start child 
//  threads, to hold the list of workers, and to read and send messages:

int main(void)
{
	zsock_t *frontend = zsock_new(ZMQ_ROUTER);
	zsock_t *backend = zsock_new(ZMQ_ROUTER);

	// IPC doesn't yet work on MS Windows.
#if (defined (WIN32))
	zsock_bind(frontend, "tcp://*:5672");
	zsock_bind(backend, "tcp://*:5673");
#else
	zsock_bind(frontend, "ipc://frontend.ipc");
	zsock_bind(backend, "ipc://backend.ipc");
#endif

	int actor_nbr = 0;
	zactor_t *actors[NBR_CLIENTS + NBR_WORKERS];

	int client_nbr;
	for (client_nbr = 0; client_nbr < NBR_CLIENTS; client_nbr++)
		actors[actor_nbr++] = zactor_new(client_task, NULL);
	int worker_nbr;
	for (worker_nbr = 0; worker_nbr < NBR_WORKERS; worker_nbr++)
		actors[actor_nbr++] = zactor_new(worker_task, NULL);

	//  Queue of available workers
	zlist_t *workers = zlist_new();

	//  .split main load-balancer loop
	//  Here is the main loop for the load balancer. It works the same way
	//  as the previous example, but is a lot shorter because CZMQ gives
	//  us an API that does more with fewer calls:
	zpoller_t *poll1 = zpoller_new(backend, NULL);
	zpoller_t *poll2 = zpoller_new(backend, frontend, NULL);
	while (true) {
		//  Poll frontend only if we have available workers
		zpoller_t *poll = zlist_size(workers) ? poll2 : poll1;
		zsock_t *ready = zpoller_wait(poll, -1);
		if (ready == NULL)
			break;              //  Interrupted

		//	Handle worker activity on backend
		if (ready == backend) {
			//  Use worker identity for load-balancing
			zmsg_t *msg = zmsg_recv(backend);
			if (!msg)
				break;          //  Interrupted

#if 0
			// zmsg_unwrap is DEPRECATED as over-engineered, poor style
			zframe_t *identity = zmsg_unwrap(msg);
#else
			zframe_t *identity = zmsg_pop(msg);
			zframe_t *delimiter = zmsg_pop(msg);
			zframe_destroy(&delimiter); 
#endif

			zlist_append(workers, identity);

			//  Forward message to client if it's not a READY
			zframe_t *frame = zmsg_first(msg);
			if (memcmp(zframe_data(frame), WORKER_READY, strlen(WORKER_READY)) == 0) {
				zmsg_destroy(&msg);
			} else {
				zmsg_send(&msg, frontend);
				if (--client_nbr == 0)
					break; // Exit after N messages
			}
		}
		else if (ready == frontend) {
			//  Get client request, route to first available worker
			zmsg_t *msg = zmsg_recv(frontend);
			if (msg) {
#if 0
				// zmsg_wrap is DEPRECATED as unsafe
				zmsg_wrap(msg, (zframe_t *)zlist_pop(workers));
#else
				zmsg_pushmem(msg, NULL, 0); // delimiter
				zmsg_push(msg, (zframe_t *)zlist_pop(workers));
#endif

				zmsg_send(&msg, backend);
			}
		}
	}
	//  When we're done, clean up properly
	while (zlist_size(workers)) {
		zframe_t *frame = (zframe_t *)zlist_pop(workers);
		zframe_destroy(&frame);
	}
	zlist_destroy(&workers);

	for (actor_nbr = 0; actor_nbr < NBR_CLIENTS + NBR_WORKERS; actor_nbr++) {
		zactor_destroy(&actors[actor_nbr]);
	}

	zpoller_destroy(&poll1);
	zpoller_destroy(&poll2);
	zsock_destroy(&frontend);
	zsock_destroy(&backend);
	return 0;
}
