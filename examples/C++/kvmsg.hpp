/*  =====================================================================
 *  kvmsg - key-value message class for example applications
 *  ===================================================================== */

#ifndef __KVMSG_HPP_INCLUDED__
#define __KVMSG_HPP_INCLUDED__

#include <random>
#include <string>
#include <unordered_map>
#include <csignal>
#include <atomic>
#include <cstdlib> // for rand
#include <zmqpp/zmqpp.hpp>

using ustring = std::basic_string<unsigned char>;
class KVMsg {
public:
    KVMsg() = default;
    //  Constructor, sets sequence as provided
    KVMsg(int64_t sequence);
    //  Destructor
    ~KVMsg();
    //  Create duplicate of kvmsg
    KVMsg(const KVMsg &other);
    //  Create copy
    KVMsg& operator=(const KVMsg &other);
    //  Reads key-value message from socket, returns new kvmsg instance.
    static KVMsg* recv(zmqpp::socket_t &socket);
    //  Send key-value message to socket; any empty frames are sent as such.
    void send(zmqpp::socket_t &socket);

    //  Return key from last read message, if any, else NULL
    std::string key() const;
    //  Return sequence nbr from last read message, if any
    int64_t sequence() const;
    //  Return body from last read message, if any, else NULL
    ustring body() const;
    //  Return body size from last read message, if any, else zero
    size_t size() const;
    //  Return UUID from last read message, if any, else NULL
    std::string uuid() const;

    //  Set message key as provided
    void set_key(std::string key);
    //  Set message sequence number
    void set_sequence(int64_t sequence);
    //  Set message body
    void set_body(ustring body);
    //  Set message UUID to generated value
    void set_uuid();
    //  Set message key using printf format
    void fmt_key(const char *format, ...);
    //  Set message body using printf format
    void fmt_body(const char *format, ...);

    //  Get message property, if set, else ""
    std::string property(const std::string &name) const;
    //  Set message property
    //  Names cannot contain '='. Max length of value is 255 chars.
    void set_property(const std::string &name, const char *format, ...);

    //  Store entire kvmsg into hash map, if key/value are set
    //  Nullifies kvmsg reference, and destroys automatically when no longer
    //  needed.
    void store(std::unordered_map<std::string, KVMsg*> &hash);
    // clear the hash map, free elements
    static void clear_kvmap(std::unordered_map<std::string, KVMsg*> &hash);
    //  Dump message to stderr, for debugging and tracing
    std::string to_string();

    void encode_frames(zmqpp::message &frames);
    void decode_frames(zmqpp::message &frames);

    //  Runs self test of class
    static bool test(int verbose);

private:
    //  Message is formatted on wire as 5 frames:
    //  frame 0: key (0MQ string)
    //  frame 1: sequence (8 bytes, network order)
    //  frame 2: uuid (blob, 16 bytes)
    //  frame 3: properties (0MQ string)
    //  frame 4: body (blob)
    static constexpr uint32_t FRAME_KEY = 0;
    static constexpr uint32_t FRAME_SEQ = 1;
    static constexpr uint32_t FRAME_UUID = 2;
    static constexpr uint32_t FRAME_PROPS = 3;
    static constexpr uint32_t FRAME_BODY = 4;
    static constexpr uint32_t KVMSG_FRAMES = 5;

    std::string key_;
    int64_t sequence_{};
    std::string uuid_;
    ustring body_;
    std::unordered_map<std::string, std::string> properties_;

    bool presents_[KVMSG_FRAMES];
};

namespace {
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
}

KVMsg::KVMsg(int64_t sequence) {
    sequence_ = sequence;
    presents_[FRAME_SEQ] = true;
}

KVMsg::~KVMsg() {
    std::cout << "DEBUG: freeing key=" << key_ << std::endl;
}

KVMsg::KVMsg(const KVMsg &other) {
    std::cout << "copy construct\n";
    key_ = other.key_;
    sequence_ = other.sequence_;
    uuid_ = other.uuid_;
    body_ = other.body_;
    properties_ = other.properties_;
    for (int i = 0; i < KVMSG_FRAMES; i++) {
        presents_[i] = other.presents_[i];
    }
}

KVMsg& KVMsg::operator=(const KVMsg &other) {
    std::cout << "copy assign\n";
    key_ = other.key_;
    sequence_ = other.sequence_;
    uuid_ = other.uuid_;
    body_ = other.body_;
    properties_ = other.properties_;
    for (int i = 0; i < KVMSG_FRAMES; i++) {
        presents_[i] = other.presents_[i];
    }
    return *this;
}

// implement the static method recv
KVMsg* KVMsg::recv(zmqpp::socket_t &socket) {
    KVMsg* kvmsg = new KVMsg(-1);
    zmqpp::message frames;
    if (!socket.receive(frames)) {
        return nullptr;
    }
    kvmsg->decode_frames(frames);
    return kvmsg;
}

void KVMsg::send(zmqpp::socket_t &socket) {
    zmqpp::message frames;
    encode_frames(frames);
    socket.send(frames);
}

std::string KVMsg::key() const {
    return key_;
}

int64_t KVMsg::sequence() const {
    return sequence_;
}

ustring KVMsg::body() const {
    return body_;
}

size_t KVMsg::size() const {
    return body_.size();
}

std::string KVMsg::uuid() const {
    return uuid_;
}

void KVMsg::set_key(std::string key) {
    key_ = key;
    presents_[FRAME_KEY] = true;
}

void KVMsg::set_sequence(int64_t sequence) {
    sequence_ = sequence;
    presents_[FRAME_SEQ] = true;
}

void KVMsg::set_body(ustring body) {
    body_ = body;
    presents_[FRAME_BODY] = true;
}

void KVMsg::set_uuid() {
    uuid_ = generateUUID();
    presents_[FRAME_UUID] = true;
}

void KVMsg::fmt_key(const char *format, ...) {
    char buffer[256];
    va_list args;
    va_start(args, format);
    vsnprintf(buffer, 256, format, args);
    va_end(args);
    key_ = buffer;
    presents_[FRAME_KEY] = true;
}

void KVMsg::fmt_body(const char *format, ...) {
    char buffer[256];
    va_list args;
    va_start(args, format);
    vsnprintf(buffer, 256, format, args);
    va_end(args);
    // body_ = ustring(buffer, buffer + strlen(buffer));
    body_ = ustring((unsigned char *)buffer, strlen(buffer));
    presents_[FRAME_BODY] = true;
}

std::string KVMsg::property(const std::string &name) const {
    if (!presents_[FRAME_PROPS]) {
        return "";
    }
    auto it = properties_.find(name);
    if (it == properties_.end()) {
        return "";
    }
    return it->second;
}

void KVMsg::set_property(const std::string &name, const char *format, ...) {
    char buffer[256];
    va_list args;
    va_start(args, format);
    vsnprintf(buffer, 256, format, args);
    va_end(args);
    properties_[name] = buffer;
    presents_[FRAME_PROPS] = true;
}

void KVMsg::encode_frames(zmqpp::message &frames) {
    // assert(frames.parts() == 0);
    if (presents_[FRAME_KEY]) {
        frames.add(key_);
    } else {
        frames.add("");
    }
    if (presents_[FRAME_SEQ]) {
        frames.add(sequence_);
    } else {
        frames.add(-1);
    }
    if (presents_[FRAME_UUID]) {
        frames.add(uuid_);
    } else {
        frames.add("");
    }
    if (presents_[FRAME_PROPS]) {
        std::string props;
        for (auto &prop : properties_) {
            props += prop.first + "=" + prop.second + "\n";
        }
        frames.add(props);
    } else {
        frames.add("");
    }
    if (presents_[FRAME_BODY]) {
        frames.add_raw(body_.data(), body_.size());
    } else {
        frames.add("");
    }
}

void KVMsg::decode_frames(zmqpp::message &frames) {
    assert(frames.parts() == KVMSG_FRAMES);
    frames.get(key_, 0);
    if (!key_.empty()) {
        presents_[FRAME_KEY] = true;
    }
    frames.get(sequence_, 1);
    if (sequence_ != -1) {
        presents_[FRAME_SEQ] = true;
    }
    frames.get(uuid_, 2);
    if (!uuid_.empty()) {
        presents_[FRAME_UUID] = true;
    }
    std::string props = frames.get<std::string>(3);
    properties_.clear();
    if (!props.empty()) {
        presents_[FRAME_PROPS] = true;
        size_t pos = 0;
        while (pos < props.size()) {
            size_t end = props.find('=', pos);
            std::string name = props.substr(pos, end - pos);
            pos = end + 1;
            end = props.find('\n', pos);
            std::string value = props.substr(pos, end - pos);
            pos = end + 1;
            properties_[name] = value;
        }
    }
    char const* raw_body = frames.get<char const*>(4);
    size_t size = frames.size(4);
    if (size > 0) {
        presents_[FRAME_BODY] = true;
        body_ = ustring((unsigned char const*)raw_body, size);
    }
}

void KVMsg::store(std::unordered_map<std::string, KVMsg*> &hash) {
    if (size() == 0) {
        hash.erase(key_);
        return;
    }
    if (presents_[FRAME_KEY] && presents_[FRAME_BODY]) {
        hash[key_] = this;
    }
}

void KVMsg::clear_kvmap(std::unordered_map<std::string, KVMsg*> &hash) {
    for (auto &kv : hash) {
        delete kv.second;
        kv.second = nullptr;
    }
    hash.clear();
}

std::string KVMsg::to_string() {
    std::stringstream ss;
    ss << "key=" << key_ << ",sequence=" << sequence_ << ",uuid=" << uuid_ << std::endl;
    ss << "propes={";
    for (auto &prop : properties_) {
        ss << prop.first << "=" << prop.second << ",";
    }
    ss << "},";
    ss << "body=";
    for (auto &byte : body_) {
        ss << std::hex << byte;
    }
    return ss.str();
}

bool KVMsg::test(int verbose) {
    zmqpp::context context;
    zmqpp::socket output(context, zmqpp::socket_type::dealer);
    output.bind("ipc://kvmsg_selftest.ipc");
    zmqpp::socket input(context, zmqpp::socket_type::dealer);
    input.connect("ipc://kvmsg_selftest.ipc");

    KVMsg kvmsg(1);
    kvmsg.set_key("key");
    kvmsg.set_uuid();
    kvmsg.set_body((unsigned char *)"body");
    if (verbose) {
        std::cout << kvmsg.to_string() << std::endl;
    }
    kvmsg.send(output);

    std::unordered_map<std::string, KVMsg*> kvmap;
    kvmsg.store(kvmap);
    std::cout << "print from kvmap[key]" << std::endl;
    std::cout << kvmap["key"]->to_string() << std::endl;

    KVMsg *kvmsg_p = KVMsg::recv(input);
    if (!kvmsg_p) {
        return false;
    }
    assert(kvmsg_p->key() == "key");
    delete kvmsg_p;

    kvmsg_p = new KVMsg(2);
    kvmsg_p->set_key("key2");
    kvmsg_p->set_property("prop1", "value1");
    kvmsg_p->set_property("prop2", "value2");
    kvmsg_p->set_body((unsigned char *)"body2");
    kvmsg_p->set_uuid();
    assert(kvmsg_p->property("prop2") == "value2");
    kvmsg_p->send(output);
    delete kvmsg_p;

    kvmsg_p = KVMsg::recv(input);
    if (!kvmsg_p) {
        return false;
    }
    assert(kvmsg_p->key() == "key2");
    assert(kvmsg_p->property("prop2") == "value2");
    if (verbose) {
        std::cout << kvmsg_p->to_string() << std::endl;
    }
    delete kvmsg_p;

    std::cout << "KVMsg self test passed" << std::endl;
    return true;
}

//  ---------------------------------------------------------------------
//  Signal handling
//
//  Call s_catch_signals() in your application at startup, and then exit
//  your main loop if s_interrupted is ever 1. Works especially well with
//  zmq_poll.

static std::atomic<int> s_interrupted(0);

void s_signal_handler(int signal_value) {
    s_interrupted = 1;
}

// setting signal handler
void s_catch_signals() {
    std::signal(SIGINT, s_signal_handler);
    std::signal(SIGTERM, s_signal_handler);
}

//  Provide random number from 0..(num-1)
static int within(int num) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(0, num - 1);
    return dis(gen);
}

#endif //  Included