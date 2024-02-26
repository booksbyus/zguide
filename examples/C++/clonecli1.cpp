#include <zmq.hpp>
#include <iostream>
#include <unordered_map>

// Narendra Pratap Singh - github.com/Narendra010/

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
		zmq::message_t kvmsg;
		updates.recv(kvmsg, zmq::recv_flags::none);

		// Convert message to string and extract key-value pair
		string kvString(static_cast<char*>(kvmsg.data()), kvmsg.size());
		size_t pos = kvString.find('=');
		string key = kvString.substr(0, pos);
		string value = kvString.substr(pos + 1);
		cout << key << " --- " << value << endl;

		// Store key-value pair in map
		kvmap[key] = value;
		sequence++;
	}

	cout << "Interrupted" << endl;
	cout << sequence << " messages in" << endl;
	return 0;
}

// Narendra Pratap Singh - github.com/Narendra010/