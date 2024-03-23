/*  =====================================================================
 *  kvsimple - simple key-value message class for example applications
 *  ===================================================================== */

#ifndef __KVSIMPLE_HPP_INCLUDED__
#define __KVSIMPLE_HPP_INCLUDED__

#include "zhelpers.hpp"
#include "zmq.hpp"
#include <cstdint>
#include <iostream>
#include <optional>
#include <sstream>
#include <string>
#include <unordered_map>

using ustring = std::basic_string<unsigned char>;
struct kvmsg {
  kvmsg(std::string key, int64_t sequence, ustring body);
  kvmsg() = default;
  //  Reads key-value message from socket, returns new kvmsg instance.
  static std::optional<kvmsg> recv(zmq::socket_t &socket);
  //  Send key-value message to socket; any empty frames are sent as such.
  void send(zmq::socket_t &socket);

  //  Return key from last read message, if any, else NULL
  std::string key() const;
  //  Return sequence nbr from last read message, if any
  int64_t sequence() const;
  //  Return body from last read message, if any, else NULL
  ustring body() const;
  //  Return body size from last read message, if any, else zero
  size_t size() const;

  //  Set message key as provided
  void set_key(std::string key);
  //  Set message sequence number
  void set_sequence(int64_t sequence);
  //  Set message body
  void set_body(ustring body);

  //  Dump message to stderr, for debugging and tracing
  std::string to_string();

  //  Runs self test of class
  static bool test(int verbose);

private:
  static constexpr uint32_t kvmsg_key_max = 255;
  static constexpr uint32_t frame_key = 0;
  static constexpr uint32_t frame_seq = 1;
  static constexpr uint32_t frame_body = 2;
  static constexpr uint32_t kvmsg_frames = 3;

  std::string key_;
  ustring body_;
  int64_t sequence_{};
};

namespace {
std::optional<zmq::message_t> receive_message(zmq::socket_t &socket) {
  zmq::message_t message(0);
  message.rebuild(0);
  try {
    if (!socket.recv(message, zmq::recv_flags::none)) {
      return {};
    }
  } catch (zmq::error_t &error) {
    std::cerr << "E: " << error.what() << std::endl;
    return {};
  }
  return message;
}
} // namespace

kvmsg::kvmsg(std::string key, int64_t sequence, ustring body)
    : key_(key), body_(body), sequence_(sequence) {}

//  Reads key-value message from socket, returns new kvmsg instance.
std::optional<kvmsg> kvmsg::recv(zmq::socket_t &socket) {
  auto key_message = receive_message(socket);
  if (!key_message)
    return {};
  kvmsg msg;
  msg.set_key(
      std::string((char *)(*key_message).data(), (*key_message).size()));
  auto sequence_message = receive_message(socket);
  msg.set_sequence(*(int64_t *)(*sequence_message).data());
  if (!sequence_message)
    return {};
  auto body_message = receive_message(socket);
  if (!body_message)
    return {};
  msg.set_body(
      ustring((unsigned char *)(*body_message).data(), (*body_message).size()));
  return msg;
}

//  Send key-value message to socket; any empty frames are sent as such.
void kvmsg::send(zmq::socket_t &socket) {
  {
    zmq::message_t message;
    message.rebuild(key_.size());
    std::memcpy(message.data(), key_.c_str(), key_.size());
    socket.send(message, zmq::send_flags::sndmore);
  }
  {
    zmq::message_t message;
    message.rebuild(sizeof(sequence_));
    std::memcpy(message.data(), (void *)&sequence_, sizeof(sequence_));
    socket.send(message, zmq::send_flags::sndmore);
  }
  {
    zmq::message_t message;
    message.rebuild(body_.size());
    std::memcpy(message.data(), body_.c_str(), body_.size());
    socket.send(message, zmq::send_flags::none);
  }
}

//  Return key from last read message, if any, else NULL
std::string kvmsg::key() const { return key_; }
//  Return sequence nbr from last read message, if any
int64_t kvmsg::sequence() const { return sequence_; }
//  Return body from last read message, if any, else NULL
ustring kvmsg::body() const { return body_; }
//  Return body size from last read message, if any, else zero
size_t kvmsg::size() const { return body_.size(); }
//  Set message key as provided
void kvmsg::set_key(std::string key) { key_ = key; }
//  Set message sequence number
void kvmsg::set_sequence(int64_t sequence) { sequence_ = sequence; }
//  Set message body
void kvmsg::set_body(ustring body) { body_ = body; }

std::string kvmsg::to_string() {
  std::stringstream ss;
  ss << "key=" << key_ << ",sequence=" << sequence_ << ",body=";
  s_dump_message(ss, body_);
  return ss.str();
}
//  Dump message to stderr, for debugging and tracing

//  Runs self test of class
bool kvmsg::test(int verbose) {

  zmq::context_t context;
  zmq::socket_t output(context, ZMQ_DEALER);
  output.bind("ipc://kvmsg_selftest.ipc");
  zmq::socket_t input(context, ZMQ_DEALER);
  input.connect("ipc://kvmsg_selftest.ipc");

  kvmsg message("key", 1, (unsigned char *)"body");
  if (verbose) {
    std::cout << message.to_string()<<std::endl;
  }
  message.send(output);
  std::unordered_map<std::string, kvmsg> kvmap;
  kvmap["key"] = message;
  auto input_message_opt = kvmsg::recv(input);
  if (!input_message_opt)
    return false;
  assert((*input_message_opt).key() == "key");
  assert((*input_message_opt).sequence() == 1);
  assert((*input_message_opt).body() == (unsigned char *)"body");
  if (verbose) {
    std::cout << (*input_message_opt).to_string()<<std::endl;
  }

  return true;
}

// Main routine for running the basic test

//int main() {
// std::cout << (kvmsg::test(1) ? "SUCCESS" : "FAILURE") << std::endl;
// return 0;
//}

#endif //  Included
