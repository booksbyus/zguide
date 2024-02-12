#include <zmq.hpp>
#include <Windows.h>
#include <iostream>
#include <unordered_map>

// Narendra Pratap Singh - github.com/Narendra010/

using namespace std;

int main() {
	// Prepare our context and publisher socket
	zmq::context_t ctx(1);
	zmq::socket_t publisher(ctx, ZMQ_PUB);
	publisher.bind("tcp://*:5555");
	Sleep(5000); // Sleep for a short while to allow connections to be established

	// Initialize key-value map and sequence
	unordered_map<string,string> kvmap;
	int64_t sequence = 0;
	srand(time(NULL));

	while (true) {
		// Distribute as key-value message
		zmq::message_t kvmsg(30); // Allocate enough space for key-value message
		int key = rand() % 10000;
		int body = rand() % 1000000;
		snprintf((char *)kvmsg.data(), 30, "%d=%d", key, body); // Format key-value pair
		publisher.send(kvmsg, zmq::send_flags::none); // Send key-value message

		// Store key-value pair in map
		kvmap[to_string(key)] = to_string(body);
		sequence++;

		// Sleep for a short while before sending the next message
		Sleep(1000);
	}

	cout << "Interrupted" << endl;
	cout << sequence << " messages out" << endl;
	return 0;
}

// Narendra Pratap Singh - github.com/Narendra010/