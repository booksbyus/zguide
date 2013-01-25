/*
 * stream_sub.cc
 *
 *  Created on: Jan 9, 2013
 *      Author: mjaggi
 */

#include <iostream>
#include <fstream>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <zmq.hpp>

using namespace std;
using namespace zmq;

int main(int argc, char *argv[]) {

	try {
		context_t ctx(1);
		message_t msg(2048);
		message_t envelope(20);

		socket_t subscriber(ctx, ZMQ_SUB);
		subscriber.connect("tcp://localhost:7779");

		if (argc > 1)
			subscriber.setsockopt(ZMQ_SUBSCRIBE, argv[1], strlen(argv[1]));
		else
			subscriber.setsockopt(ZMQ_SUBSCRIBE, "", 0);

		cout << "waiting for zmq sub msg \n";

		while (1) {
			subscriber.recv(&envelope);
			subscriber.recv(&msg);
			string topic((const char*) envelope.data(), envelope.size());
			string data((const char*) msg.data(), msg.size());
			cout << "Topic: " << topic << endl;
			cout << "Data: " << data << endl;
		}
	} catch (int e) {
		cout << "Error " << e << endl;
	}

	return 0;
}

