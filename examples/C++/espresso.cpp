#include <iostream>
#include <thread>
#include <zmq.hpp>
#include <string>
#include <chrono>

// Subscriber thread function
void subscriber_thread(zmq::context_t& ctx) {
	zmq::socket_t subscriber(ctx, ZMQ_SUB);
	subscriber.connect("tcp://localhost:5555");
	subscriber.set(zmq::sockopt::subscribe, "A");
	subscriber.set(zmq::sockopt::subscribe, "B");

	int count = 0;
	while (count < 5) {
		zmq::message_t message;
		if (subscriber.recv(message)) {
			std::string msg = std::string((char*)(message.data()), message.size());
			std::cout << "Received: " << msg << std::endl;
			count++;
		}
		std::this_thread::sleep_for(std::chrono::milliseconds(100));
	}
}

// Publisher thread function
void publisher_thread(zmq::context_t& ctx) {
	zmq::socket_t publisher(ctx, ZMQ_PUB);
	publisher.bind("tcp://*:5555");

	while (true) {
		char string[10];
		sprintf_s(string, "%c-%05d", rand() % 10 + 'A', rand() % 100000);
		zmq::message_t message(string, strlen(string));
		publisher.send(message, zmq::send_flags::none);
		std::this_thread::sleep_for(std::chrono::milliseconds(100));
	}
}

// Listener thread function
void listener_thread(zmq::context_t& ctx) {
	zmq::socket_t listener(ctx, ZMQ_PAIR);
	listener.connect("inproc://listener");

	while (true) {
		zmq::message_t message;
		if (listener.recv(message)) {
			std::string msg = std::string((char*)(message.data()), message.size());
			std::cout << "Listener Received: " << msg << std::endl;
		}
	}
}

int main() {
	zmq::context_t context(1);

	// Start publisher and subscriber threads
	std::thread pub_thread(publisher_thread, std::ref(context));
	std::thread sub_thread(subscriber_thread, std::ref(context));

	// Set up listener thread
	std::thread listener_thread(listener_thread, std::ref(context));

	// Main thread acts as the listener proxy
	zmq::socket_t proxy(context, ZMQ_PAIR);
	proxy.bind("inproc://listener");

	// Proxy messages between SUB and PUB sockets
	zmq_proxy(zmq::socket_t(context, ZMQ_XSUB), zmq::socket_t(context, ZMQ_XPUB), proxy);

	// Wait for threads to finish
	pub_thread.join();
	sub_thread.join();
	listener_thread.join();

	return 0;
}
