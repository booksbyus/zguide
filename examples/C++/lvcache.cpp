//  Last value cache
//  Uses XPUB subscription messages to re-send data

#include <unordered_map>
#include "zhelpers.hpp"

int main ()
{
    zmq::context_t context(1);
    zmq::socket_t  frontend(context, ZMQ_SUB);
    zmq::socket_t  backend(context, ZMQ_XPUB);

    frontend.connect("tcp://localhost:5557");
    backend.bind("tcp://*:5558");

    //  Subscribe to every single topic from publisher
    frontend.setsockopt(ZMQ_SUBSCRIBE, "", 0);

    //  Store last instance of each topic in a cache
    std::unordered_map<std::string, std::string> cache_map;

    zmq::pollitem_t items[2] = {
        { static_cast<void*>(frontend), 0, ZMQ_POLLIN, 0 },
        { static_cast<void*>(backend), 0, ZMQ_POLLIN, 0 }
    };

    //  .split main poll loop
    //  We route topic updates from frontend to backend, and we handle
    //  subscriptions by sending whatever we cached, if anything:
    while (1)
    {
        if (zmq::poll(items, 2, 1000) == -1)
            break; //  Interrupted

        //  Any new topic data we cache and then forward
        if (items[0].revents & ZMQ_POLLIN)
        {
            std::string topic = s_recv(frontend);
            std::string data  = s_recv(frontend);

            if (topic.empty())
                break;

            cache_map[topic] = data;

            s_sendmore(backend, topic);
            s_send(backend, data);
        }

        //  .split handle subscriptions
        //  When we get a new subscription, we pull data from the cache:
        if (items[1].revents & ZMQ_POLLIN) {
            zmq::message_t msg;

            backend.recv(&msg);
            if (msg.size() == 0)
                break;

            //  Event is one byte 0=unsub or 1=sub, followed by topic
            uint8_t *event = (uint8_t *)msg.data();
            if (event[0] == 1) {
                std::string topic((char *)(event+1), msg.size()-1);

                auto i = cache_map.find(topic);
                if (i != cache_map.end())
                {
                    s_sendmore(backend, topic);
                    s_send(backend, i->second);
                }
            }
        }
    }

    return 0;
}
