//
// created by Jinyang Shao on 9/3/2024
//

#include "mdcliapi.hpp"


static zmsg* 
s_service_call(mdcli* session, std::string service, zmsg *&request) {
    zmsg* reply = session->send(service, request);
    if (reply) {
        std::string status = (char *)reply->pop_front().c_str();
        if (status.compare("200") == 0) {
            return reply;
        } else if (status.compare("400") == 0) {
            std::cout << "E: client fatal error, aborting\n" << std::endl;
            exit(EXIT_FAILURE);
        } else if (status.compare("500") == 0) {
            std::cout << "E: server fatal error, aborting\n" << std::endl;
            exit(EXIT_FAILURE);
        }
    } else {
        exit(EXIT_SUCCESS); // interrupted or failed
    }

    delete reply;
    return nullptr; //  Didn't succeed; don't care why not
}

//  .split main task
//  The main task tests our service call by sending an echo request:

int main (int argc, char *argv [])
{
    int verbose = (argc > 1 && strcmp (argv [1], "-v") == 0);
    mdcli session("tcp://localhost:5555", verbose);
    session.set_timeout(3000);
    session.set_retries(10);

    // 1. Send 'echo' request to Titanic
    zmsg *request = new zmsg("Hello world");
    request->push_front("echo");
    zmsg *reply = s_service_call(&session, "titanic.request", request);

    ustring uuid;
    if (reply) {
        uuid = reply->pop_front();
        delete reply;
        std::cout << "I: request UUID: " << uuid.c_str() << std::endl;
    }

    //  2. Wait until we get a reply
    while (!s_interrupted) {
        s_sleep(100); // 100 ms
        request = new zmsg((char *)uuid.c_str());
        zmsg *reply = s_service_call(&session, "titanic.reply", request);

        if (reply) {
            std::cout << "Reply: " << reply->body() << std::endl;
            delete reply;

            // 3. Close request
            request = new zmsg((char *)uuid.c_str());
            reply = s_service_call(&session, "titanic.close", request);
            delete reply;
            break;
        } else {
            std::cout << "I: no reply yet, trying again...\n" << std::endl;
            s_sleep(5000); // 5 sec
        }
    }
    return 0;
}