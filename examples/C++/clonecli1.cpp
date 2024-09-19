#include <iostream>
#include <unordered_map>
#include "kvsimple.hpp"

using namespace std;

int main() {
	// Prepare our context and updates socket
	zmq::context_t ctx(1);
	zmq::socket_t updates(ctx, ZMQ_SUB);
	updates.set(zmq::sockopt::subscribe, ""); // Subscribe to all messages
	updates.connect("tcp://localhost:5555");

	// Initialize key-value map and sequence
	unordered_map<string, string> kvmap;
	int64_t sequence = 0;

	while (true) {
		// Receive key-value message
		auto update_kv_msg = kvmsg::recv(updates);
		if (!update_kv_msg) {
			cout << "Interrupted" << endl;
			return 0;
		}

		// Convert message to string and extract key-value pair
		string key = update_kv_msg->key();
		string value = (char *)update_kv_msg->body().c_str();
		cout << key << " --- " << value << endl;

		// Store key-value pair in map
		kvmap[key] = value;
		sequence++;
	}
	return 0;
}
