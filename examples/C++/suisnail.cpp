//
// Suicidal Snail
//
// Andreas Hoelzlwimmer <andreas.hoelzlwimmer@fh-hagenberg.at>
#include "zhelpers.hpp"
#include <thread>

// ---------------------------------------------------------------------
// This is our subscriber
// It connects to the publisher and subscribes to everything. It
// sleeps for a short time between messages to simulate doing too
// much work. If a message is more than 1 second late, it croaks.

#define MAX_ALLOWED_DELAY 1000 // msecs

namespace {
	bool Exit = false;
};

static void *
subscriber () {
    zmq::context_t context(1);

    // Subscribe to everything
    zmq::socket_t subscriber(context, ZMQ_SUB);
    subscriber.connect("tcp://localhost:5556");
    subscriber.set(zmq::sockopt::subscribe, "");

    std::stringstream ss;
    // Get and process messages
    while (1) {
        ss.clear();
        ss.str(s_recv (subscriber));
        int64_t clock;
        assert ((ss >> clock));
	const auto delay = s_clock () - clock;
        // Suicide snail logic
        if (delay> MAX_ALLOWED_DELAY) {
            std::cerr << "E: subscriber cannot keep up, aborting. Delay=" <<delay<< std::endl;
            break;
        }
        // Work for 1 msec plus some random additional time
        s_sleep(1000*(1+within(2)));
    }
    Exit = true;
    return (NULL);
}


// ---------------------------------------------------------------------
// This is our server task
// It publishes a time-stamped message to its pub socket every 1ms.

static void *
publisher () {
    zmq::context_t context (1);

    // Prepare publisher
    zmq::socket_t publisher(context, ZMQ_PUB);
    publisher.bind("tcp://*:5556");

    std::stringstream ss;

    while (!Exit) {
        // Send current clock (msecs) to subscribers
        ss.str("");
        ss << s_clock();
        s_send (publisher, ss.str());

        s_sleep(1);
    }
    return 0;
}


// This main thread simply starts a client, and a server, and then
// waits for the client to croak.
//
int main (void)
{
    std::thread server_thread(&publisher);
    std::thread client_thread(&subscriber);
    client_thread.join();
    server_thread.join();
    return 0;
}

