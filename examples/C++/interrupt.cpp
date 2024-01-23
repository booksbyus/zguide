//  Handling Interrupt Signals in C++
//
//  Saad Hussain <saadnasir31@gmail.com>

#include <csignal>
#include <iostream>
#include <zmq.hpp>

int interrupted{0};

void signal_handler(int signal_value) { interrupted = 1; }

void catch_signals() {
  std::signal(SIGINT, signal_handler);
  std::signal(SIGTERM, signal_handler);
  std::signal(SIGSEGV, signal_handler);
  std::signal(SIGABRT, signal_handler);
}

int main() {
  zmq::context_t ctx(1);

  zmq::socket_t socket(ctx, ZMQ_REP);
  socket.bind("tcp://localhost:5555");

  catch_signals();
  while (true) {
    zmq::message_t msg;

    try {
      socket.recv(&msg);
    } catch (zmq::error_t &e) {
      std::cout << "interrupt received, proceeding..." << std::endl;
    }

    if (interrupted) {
      std::cout << "interrupt received, killing program..." << std::endl;
      break;
    }
  }
  return 0;
}