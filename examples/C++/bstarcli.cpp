//  Binary Star client proof-of-concept implementation. This client does no
//  real work; it just demonstrates the Binary Star failover model.

#include "zmsg.hpp"
#define REQUEST_TIMEOUT     1000    //  msecs
#define SETTLE_DELAY        2000    //  Before failing over
#define ZMQ_POLL_MSEC       1       //  zmq_poll delay

int main(void) {
    zmq::context_t context(1);
    char *server [] = {"tcp://localhost:5001", "tcp://localhost:5002"};
    uint server_nbr = 0;

    std::cout << "I: connecting to " << server[server_nbr] << "..." << std::endl;
    zmq::socket_t *client = new zmq::socket_t(context, ZMQ_REQ);
    // Configure socket to not wait at close time
    int linger = 0;
    client->setsockopt (ZMQ_LINGER, &linger, sizeof (linger));
    client->connect(server[server_nbr]);

    int sequence = 0;
    while(true) {
        //  We send a request, then we work to get a reply
        std::string request_string = std::to_string(++sequence);
        s_send(*client, request_string);

        int expect_reply = 1;
        while(expect_reply) {
            zmq::pollitem_t items[] = {{*client, 0, ZMQ_POLLIN, 0}};
            try {
                zmq::poll(items, 1, REQUEST_TIMEOUT * ZMQ_POLL_MSEC);
            } catch (std::exception &e) {
                break;              //  Interrupted
            }

            //  .split main body of client
            //  We use a Lazy Pirate strategy in the client. If there's no
            //  reply within our timeout, we close the socket and try again.
            //  In Binary Star, it's the client vote that decides which
            //  server is primary; the client must therefore try to connect
            //  to each server in turn:
            if (items[0].revents & ZMQ_POLLIN) {
                //  We got a reply from the server, must match sequence
                std::string reply = s_recv(*client);
                if (std::stoi(reply) == sequence) {
                    std::cout << "I: server replied OK (" << reply << ")" << std::endl;
                    expect_reply = 0;
                    s_sleep(1000);  //  One request per second
                } else {
                    std::cout << "E: bad reply from server: " << reply << std::endl;
                }
            } else {
                std::cout << "W: no response from server, failing over" << std::endl;

                //  Old socket is confused; close it and open a new one
                delete client;
                server_nbr = (server_nbr + 1) % 2;
                s_sleep(SETTLE_DELAY);
                std::cout << "I: connecting to " << server[server_nbr] << "..." << std::endl;
                client = new zmq::socket_t(context, ZMQ_REQ);
                linger = 0;
                client->setsockopt(ZMQ_LINGER, &linger, sizeof(linger));
                client->connect(server[server_nbr]);

                //  Send request again, on new socket
                s_send(*client, request_string);
            }
        }
    }
    return 0;
}