//
//   Request-reply service in C++
//   Connects REP socket to tcp://localhost:5560
//   Expects "Hello" from client, replies with "World"
//
//  Olivier Chamoux <olivier.chamoux@fr.thalesgroup.com>
//
#include <zmq.hpp>
#include <iostream>

int main (int argc, char *argv[])
{
    zmq::context_t context(1);

	zmq::socket_t socket(context, ZMQ_REP);
	socket.connect("tcp://localhost:5560");

	while(1)
	{
		zmq::message_t message;
		socket.recv(&message);

		std::cout << "Received request: "
				<< static_cast<char*>(message.data()) << std::endl;

		message.rebuild(6);
		memcpy(message.data(), "World\0", 6);
		socket.send(message);

	}
}

