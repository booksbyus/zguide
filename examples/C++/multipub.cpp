/*
 * stream_multipub.cc
 *
 *  Created on: Jan 15, 2013
 *      Author: mjaggi
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <iostream>
#include <fstream>
#include <string>
#include <zmq.hpp>
#include <stdlib.h>

using namespace std;
using namespace zmq;

__thread socket_t *tls_publisher; //thread local publisher

context_t g_ctx(1);
socket_t g_xpub(g_ctx, ZMQ_XPUB);
socket_t g_xsub(g_ctx, ZMQ_XSUB);

/*
 * forwarder from xsub(to which publishers connect)
 * to xpub(to which subscribers connect)
 * does not return
 */

static void *pubsub_proxy_thread(void *arg) {
	zmq_proxy(g_xsub, g_xpub, NULL);
	return NULL;
}

int init() {
	pthread_t t;

	try {
		g_xpub.bind("tcp://*:7779"); //subscribers connect here
		g_xsub.bind("inproc://xsub_sock");//publishers connect here

		assert(0 == pthread_create(&t, NULL, pubsub_proxy_thread, NULL));
	}
	catch(const exception& e) {
		cerr << "Exception: " << e.what() << endl;
		return -1;
	}
	return 0;
}

/*
 *
 */
int thread_init() {
	try {
		socket_t *publisher = new socket_t(g_ctx, ZMQ_PUB);
		publisher->connect("inproc://xsub_sock");
		tls_publisher = publisher;
	} catch (const exception& e) {
		cerr << "Exception: " << e.what() << endl;
		return -1;
	}
	return 0;
}

/*
 *
 */
int publish(string topic, string data)
 {
	try {
		message_t *m = new message_t(2048);
		tls_publisher->send(topic.c_str(), topic.size(), ZMQ_SNDMORE); //topic
		tls_publisher->send(data.c_str(), data.size()); //topic
	} catch (const exception &e) {
		cerr << "Exception: " << e.what() << endl;
		return -1;
	}
	return 0;

}

#define within(num) (int) ((float) num * random () / (RAND_MAX + 1.0))

void *publisher(void *arg) {
	thread_init();
	while (1) {
		switch(within(2)) {
			case 0: 
				cout << "publishing A" << endl;
				publish("A", "Adata");
				break;
			case 1: 
				cout << "publishing B" << endl;
				publish("B", "Bdata");
				break;
			case 2:
				cout << "publishing C" << endl;
				publish("C", "Cdata");
				break;
		}
		sleep(5);
	}
	return NULL;
}

int main(int argc, char *argv[]) {
	pthread_t t1, t2;
	init();
	assert(0 == pthread_create(&t1, NULL, publisher, NULL));
	assert(0 == pthread_create(&t2, NULL, publisher, NULL));

	while (1)
		;

	return 0;
}

