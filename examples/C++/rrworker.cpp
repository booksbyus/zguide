//
//   Request-reply service in C++
//   Connects REP socket to tcp://localhost:5560
//   Expects "Hello" from client, replies with "World"
//


#include "zhelpers.hpp"
 
int main (int argc, char *argv[])
{
    zmq::context_t context(1);

	zmq::socket_t responder(context, ZMQ_REP);
	responder.connect("tcp://localhost:5560");
 
	while(1)
	{
		//  Wait for next request from client
		std::string string = s_recv (responder);
		
		std::cout << "Received request: " << string << std::endl;
		
		// Do some 'work'
        sleep (1);
        
        //  Send reply back to client
		s_send (responder, std::string("World"));
		
	}
}

