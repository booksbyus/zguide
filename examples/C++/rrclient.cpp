//  Request-reply client in C++
//  Connects REQ socket to tcp://localhost:5559
//  Sends "Hello" to server, expects "World" back
//
//  Olivier Chamoux <olivier.chamoux@fr.thalesgroup.com>
//
#include <zmq.hpp>
#include <iostream>

int main (int argc, char *argv[])
{
    zmq::context_t context(1);

	zmq::socket_t socket(context, ZMQ_REQ);
	socket.connect("tcp://localhost:5559");

	for( int request = 0 ; request < 10 ; request++) {

		zmq::message_t message(6);
		memcpy(message.data(), "Hello\0", 6);
		socket.send(message);

		message.rebuild();
		socket.recv(&message);

		std::cout << "Received reply " << request
				<< "[" << static_cast<char*>(message.data()) << "]"
				<< std::endl;
	}
}
