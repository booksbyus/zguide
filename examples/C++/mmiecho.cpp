#include "mdcliapi.hpp"

int main(int argc, char **argv) {

  int verbose = (argc > 1 && strcmp(argv[1], "-v")== 0);

  mdcli session("tcp://localhost:5555", verbose);

  zmsg* request = new zmsg("echo");
  zmsg *reply = session.send("mmi.service", request);
  if (reply) {
    ustring status = reply->pop_front();
    std::cout << "Lookup echo service: " <<(char *) status.c_str() << std::endl;
    delete reply;
  }
  return 0;
}
