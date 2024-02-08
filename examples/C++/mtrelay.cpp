/*
author: Saad Hussain <saadnasir31@gmail.com>
*/

#include <iostream>
#include <thread>
#include <zmq.hpp>

void step1(zmq::context_t &context) {
  // Connect to step2 and tell it we're ready
  zmq::socket_t xmitter(context, zmq::socket_type::pair);
  xmitter.connect("inproc://step2");

  std::cout << "Step 1 ready, signaling step 2" << std::endl;

  zmq::message_t msg("READY");
  xmitter.send(msg, zmq::send_flags::none);
}

void step2(zmq::context_t &context) {
  // Bind inproc socket before starting step1
  zmq::socket_t receiver(context, zmq::socket_type::pair);
  receiver.bind("inproc://step2");

  std::thread thd(step1, std::ref(context));

  // Wait for signal and pass it on
  zmq::message_t msg;
  receiver.recv(msg, zmq::recv_flags::none);

  // Connect to step3 and tell it we're ready
  zmq::socket_t xmitter(context, zmq::socket_type::pair);
  xmitter.connect("inproc://step3");

  std::cout << "Step 2 ready, signaling step 3" << std::endl;

  xmitter.send(zmq::str_buffer("READY"), zmq::send_flags::none);
  thd.join();
}

int main() {
  zmq::context_t context(1);

  // Bind inproc socket before starting step2
  zmq::socket_t receiver(context, zmq::socket_type::pair);
  receiver.bind("inproc://step3");

  std::thread thd(step2, std::ref(context));

  // Wait for signal
  zmq::message_t msg;
  receiver.recv(msg, zmq::recv_flags::none);

  std::cout << "Test successful!" << std::endl;
  thd.join();
  return 0;
}
