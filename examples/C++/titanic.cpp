#include <iostream>
#include <random>
#include <sstream>
#include <iomanip>
#include <thread>
#include <filesystem>
#include <fstream>

#include "mdcliapi.hpp"
#include "mdwrkapi.hpp"

#define ZMQ_POLL_MSEC 1
#define BROKER_ENDPOINT "tcp://localhost:5555"

std::string generateUUID() {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(0, 15);
    std::uniform_int_distribution<> dis2(8, 11);

    std::stringstream ss;
    ss << std::hex;
    for (int i = 0; i < 8; ++i) ss << dis(gen);
    // ss << "-";
    for (int i = 0; i < 4; ++i) ss << dis(gen);
    ss << "4";  // UUID version 4
    for (int i = 0; i < 3; ++i) ss << dis(gen);
    // ss << "-";
    ss << dis2(gen);  // UUID variant
    for (int i = 0; i < 3; ++i) ss << dis(gen);
    // ss << "-";
    for (int i = 0; i < 12; ++i) ss << dis(gen);
    return ss.str();
}

//  Returns freshly allocated request filename for given UUID

#define TITANIC_DIR ".titanic"

static std::string s_request_filename(std::string uuid) {
    return std::string(TITANIC_DIR) + "/" + uuid + ".req";
}

//  Returns freshly allocated reply filename for given UUID
static std::string s_reply_filename(std::string uuid) {
    return std::string(TITANIC_DIR) + "/" + uuid + ".rep";
}

static bool s_zmsg_save(zmsg *msg, const std::filesystem::path &filepath) {
    std::ofstream ofs(filepath);
    if (ofs) {
        while (msg->parts() > 0)
        {
            ofs << msg->pop_front().c_str() << std::endl;
        }

        std::cout << "File created and data written: " << filepath << std::endl;
        return true;
    }
    else {
        std::cerr << "Failed to create or write to file: " << filepath << std::endl;
        return false;
    }
}

static zmsg* s_zmsg_load(const std::filesystem::path &filepath) {
    zmsg *msg = new zmsg();
    std::ifstream ifs(filepath);
    if (ifs) {
        std::string line;
        while (std::getline(ifs, line)) {
            msg->push_back(line.c_str());
        }
        ifs.close();
        return msg;
    }
    else {
        std::cerr << "Failed to read file: " << filepath << std::endl;
        return nullptr;
    }
}

//  .split Titanic request service
//  The {{titanic.request}} task waits for requests to this service. It writes
//  each request to disk and returns a UUID to the client. The client picks
//  up the reply asynchronously using the {{titanic.reply}} service:
static void titanic_request(zmq::context_t *ctx) {
    mdwrk *worker = new mdwrk(BROKER_ENDPOINT, "titanic.request", 0);
    worker->set_heartbeat(3000);
    zmsg *reply = nullptr;
    // communicate with parent thread
    zmq::socket_t pipe(*ctx, ZMQ_PAIR);
    pipe.bind("inproc://titanic_request");

    while (true) {
        //  Send reply if it's not null
        //  And then get next request from broker
        zmsg *request = worker->recv(reply);
        std::cout << "titanic_request: received request" << std::endl;
        request->dump();
        if (!request) {
            break; //  Interrupted, exit
        }
        //  Ensure message directory exists
        std::filesystem::path titanic_dir(TITANIC_DIR);
        std::filesystem::create_directory(titanic_dir);
        // std::cout << "I: creating " << TITANIC_DIR << " directory" << std::endl;

        //  Generate UUID and save message to disk
        std::string uuid = generateUUID();

        std::filesystem::path request_file(s_request_filename(uuid));
        if (!s_zmsg_save(request, request_file)) {
            break; //  dump file failed, exit
        }
        delete request;

        //  Send UUID through to message queue
        reply = new zmsg(uuid.c_str());
        reply->send(pipe);
        std::cout << "titanic_request: sent reply to parent" << std::endl;

        //  Now send UUID back to client
        //  Done by the mdwrk_recv() at the top of the loop
        reply = new zmsg("200");
        reply->push_back(uuid.c_str());
    }
    delete worker;
    return;
}

//  .split Titanic reply service
//  The {{titanic.reply}} task checks if there's a reply for the specified
//  request (by UUID), and returns a 200 (OK), 300 (Pending), or 400
//  (Unknown) accordingly:

static void titanic_reply(zmq::context_t *ctx) {
    mdwrk *worker = new mdwrk(BROKER_ENDPOINT, "titanic.reply", 0);
    worker->set_heartbeat(3000);
    zmsg *reply = nullptr;

    while(true) {
        zmsg *request = worker->recv(reply);
        if (!request) {
            break; //  Interrupted, exit
        }
        std::string uuid = std::string((char *)request->pop_front().c_str());
        std::filesystem::path request_filename(s_request_filename(uuid));
        std::filesystem::path reply_filename(s_reply_filename(uuid));

        // Try to read the reply file
        if (std::filesystem::exists(reply_filename)) {
            reply = s_zmsg_load(reply_filename);
            assert(reply);
            reply->push_front("200");
        }
        else {
            reply = new zmsg();
            if (std::filesystem::exists(request_filename)) {
                reply->push_front("300"); //Pending
            }
            else {
                reply->push_front("400"); //Unknown
            }
        }
        delete request;
    }
    delete worker;
}

//  .split Titanic close task
//  The {{titanic.close}} task removes any waiting replies for the request
//  (specified by UUID). It's idempotent, so it is safe to call more than
//  once in a row:
static void titanic_close(zmq::context_t *ctx) {
    mdwrk *worker = new mdwrk(BROKER_ENDPOINT, "titanic.close", 0);
    worker->set_heartbeat(3000);
    zmsg *reply = nullptr;

    while (true) {
        zmsg *request = worker->recv(reply);
        if (!request) {
            break; //  Interrupted, exit
        }

        std::string uuid = std::string((char *)request->pop_front().c_str());
        std::filesystem::path request_filename(s_request_filename(uuid));
        std::filesystem::path reply_filename(s_reply_filename(uuid));
        std::filesystem::remove(request_filename);
        std::filesystem::remove(reply_filename);

        delete request;
        reply = new zmsg("200");
    }
    delete worker;
    return;
}

//  .split worker task
//  This is the main thread for the Titanic worker. It starts three child
//  threads; for the request, reply, and close services. It then dispatches
//  requests to workers using a simple brute force disk queue. It receives
//  request UUIDs from the {{titanic.request}} service, saves these to a disk
//  file, and then throws each request at MDP workers until it gets a
//  response.

static bool s_service_success(std::string uuid);

// simulate zthread_fork, create attached thread and return the pipe socket
std::pair<std::thread, zmq::socket_t> zthread_fork(zmq::context_t& ctx, void (*thread_func)(zmq::context_t*)) {
    // create the pipe socket for the main thread to communicate with its child thread
    zmq::socket_t pipe(ctx, ZMQ_PAIR);
    pipe.connect("inproc://titanic_request");

    // start child thread
    std::thread t(thread_func, &ctx);

    return std::make_pair(std::move(t), std::move(pipe));
}

int main(int argc, char *argv[]) {
    // std::string uuid = generateUUID();
    // std::cout << "Generated UUID: " << uuid << std::endl;
    // return 0;
    int verbose = (argc > 1 && strcmp(argv[1], "-v") == 0);
    zmq::context_t ctx(1);

    // start the child threads
    auto [titanic_request_thread, request_pipe] = zthread_fork(ctx, titanic_request);
    std::thread titanic_reply_thread(titanic_reply, &ctx);
    titanic_reply_thread.detach();
    std::thread titanic_close_thread(titanic_close, &ctx);
    titanic_close_thread.detach();
    if (verbose) {
        std::cout << "I: all service threads started(request, reply, close)" << std::endl;
    }

    // Main dispatcher loop
    while (true) {
        //  We'll dispatch once per second, if there's no activity
        zmq::pollitem_t items[] = {
            {request_pipe, 0, ZMQ_POLLIN, 0}
        };
        try {
            zmq::poll(items, 1, 1000 * ZMQ_POLL_MSEC);
        } catch(...) {
            break; //  Interrupted
        }
        std::filesystem::path titanic_dir(TITANIC_DIR);

        if (items[0].revents & ZMQ_POLLIN) {
            //  Ensure message directory exists
            std::cout << "I: creating " << TITANIC_DIR << " directory" << std::endl;
            std::filesystem::create_directory(titanic_dir);
            
            //  Append UUID to queue, prefixed with '-' for pending
            zmsg *msg = new zmsg(request_pipe);
            if (!msg) {
                break; //  Interrupted
            }
            std::ofstream ofs(titanic_dir / "queue", std::ios::app); // create if not exist, append
            std::string uuid = std::string((char *)msg->pop_front().c_str());
            ofs << "-" << uuid << std::endl;
            delete msg;
        }
        //  Brute force dispatcher
        // std::array<char, 33> entry; // "?.......:.......:.......:.......:"
        std::string line;
        bool need_commit = false;
        std::vector<std::string> new_lines;
        std::ifstream file(titanic_dir / "queue");

        if (!file.is_open()) {
            if (verbose) {
                std::cout << "I: queue file not open" << std::endl;
            }
            continue;
        }
        if (verbose) {
            std::cout << "I: read from queue file" << std::endl;
        }
        while (std::getline(file, line)) {
            if (line[0] == '-') {
                std::string uuid = line.substr(1, 32);
                if (verbose) {
                    std::cout << "I: processing request " << uuid << std::endl;
                }
                if (s_service_success(uuid)) {
                    line[0] = '+'; //  Mark completed
                    need_commit = true;
                }
            }
            new_lines.push_back(line);
        }
        file.close();
        // Commit update
        if (need_commit) {
            std::ofstream outfile(titanic_dir / "queue");
            if (!outfile.is_open()) {
                std::cerr << "I: unable to open queue file" << std::endl;
                return 1;
            }
            for (const auto &line : new_lines) {
                outfile << line << std::endl;
            }
            outfile.close();
        }
    }
    return 0;
}

//  .split try to call a service
//  Here, we first check if the requested MDP service is defined or not,
//  using a MMI lookup to the Majordomo broker. If the service exists,
//  we send a request and wait for a reply using the conventional MDP
//  client API. This is not meant to be fast, just very simple:
static bool s_service_success(std::string uuid) {
    //  Load request message, service will be first frame
    std::filesystem::path request_filepath(s_request_filename(uuid));
    std::ifstream ifs(request_filepath);

    //  If the client already closed request, treat as successful
    if (!ifs) {
        return 1;
    }

    zmsg *request = s_zmsg_load(request_filepath);
    char* service_name = (char *)request->pop_front().c_str();

    //  Create MDP client session with short timeout
    mdcli client(BROKER_ENDPOINT, 1);
    client.set_timeout(1000); // 1 sec
    client.set_retries(1);

    //  Use MMI protocol to check if service is available
    zmsg *mmi_request = new zmsg();
    mmi_request->push_back(service_name);
    zmsg *mmi_reply = client.send("mmi.service", mmi_request);

    bool service_ok = (mmi_reply && strcmp(mmi_reply->address(), "200")==0);
    delete mmi_reply;

    bool result = false;
    if (service_ok) {
        zmsg *reply = client.send(service_name, request);
        if (reply) {
            std::filesystem::path reply_filepath(s_reply_filename(uuid));
            s_zmsg_save(reply, reply_filepath);
            result = true;
        }
        delete reply;
    } else {
        std::cout << "service not available: " << service_name << std::endl;
        delete request;
    }
    return result;
}