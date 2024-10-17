#ifndef __ZHELPERS_HPP_INCLUDED__
#define __ZHELPERS_HPP_INCLUDED__

//  Include a bunch of headers that we will need in the examples

#include <stdexcept>
#include <zmq.hpp> // https://github.com/zeromq/cppzmq

#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>

#include <time.h>
#include <assert.h>
#include <stdlib.h>        // random()  RAND_MAX
#include <stdio.h>
#include <stdarg.h>
#include <signal.h>
#if (!defined(WIN32))
#   include <sys/time.h>
#   include <unistd.h>
#endif

//  Bring Windows MSVC up to C99 scratch
#if (defined (WIN32))
    typedef unsigned long ulong;
    typedef unsigned int  uint;
    typedef __int64 int64_t;
#endif

//  On some version of Windows, POSIX subsystem is not installed by default.
//  So define srandom and random ourself.
//  
#if (defined (WIN32))
#   define srandom srand
#   define random rand
#endif

// Visual Studio versions below 2015 do not support sprintf properly. This is a workaround.
// Taken from http://stackoverflow.com/questions/2915672/snprintf-and-visual-studio-2010
#if defined(_MSC_VER) && _MSC_VER < 1900

#define snprintf c99_snprintf
#define vsnprintf c99_vsnprintf

	inline int c99_vsnprintf(char *outBuf, size_t size, const char *format, va_list ap)
	{
		int count = -1;

		if (size != 0)
			count = _vsnprintf_s(outBuf, size, _TRUNCATE, format, ap);
		if (count == -1)
			count = _vscprintf(format, ap);

		return count;
	}

	inline int c99_snprintf(char *outBuf, size_t size, const char *format, ...)
	{
		int count;
		va_list ap;

		va_start(ap, format);
		count = c99_vsnprintf(outBuf, size, format, ap);
		va_end(ap);

		return count;
	}

#endif

//  Provide random number from 0..(num-1)
#define within(num) (int) ((float)((num) * random ()) / (RAND_MAX + 1.0))

//  Receive 0MQ string from socket and convert into C string
//  Caller must free returned string.
inline static char *
s_recv(void *socket, int flags = 0) {
	zmq_msg_t message;
	zmq_msg_init(&message);

	int rc = zmq_msg_recv(&message, socket, flags);

	if (rc < 0)
		return nullptr;           //  Context terminated, exit

	size_t size = zmq_msg_size(&message);
	char *string = (char*)malloc(size + 1);
	memcpy(string, zmq_msg_data(&message), size);
	zmq_msg_close(&message);
	string[size] = 0;
	return (string);
}

//  Receive 0MQ string from socket and convert into string
inline static std::string
s_recv (zmq::socket_t & socket, zmq::recv_flags flags = zmq::recv_flags::none) {

    zmq::message_t message;
	zmq::recv_result_t rc = socket.recv(message, flags);
	if (rc) {
		return std::string(static_cast<char*>(message.data()), message.size());
	} else {
		return "";
	}

}

inline static bool s_recv(zmq::socket_t & socket, std::string & ostring, zmq::recv_flags flags = zmq::recv_flags::none)
{
	zmq::message_t message;
	zmq::recv_result_t rc = socket.recv(message, flags);

	if (rc) {
		ostring = std::string(static_cast<char*>(message.data()), message.size());
	}
	
	return bool(rc);
}

//  Convert C string to 0MQ string and send to socket
inline static int
s_send(void *socket, const char *string, int flags = 0) {
	int rc;
	zmq_msg_t message;
	zmq_msg_init_size(&message, strlen(string));
	memcpy(zmq_msg_data(&message), string, strlen(string));
	rc = zmq_msg_send(&message, socket, flags);
	assert(-1 != rc);
	zmq_msg_close(&message);
	return (rc);
}

//  Convert string to 0MQ string and send to socket
inline static bool
s_send (zmq::socket_t & socket, const std::string & string, zmq::send_flags flags = zmq::send_flags::none) {

    zmq::message_t message(string.size());
    memcpy (message.data(), string.data(), string.size());

	zmq::recv_result_t rc = socket.send (message, flags);
    return bool(rc);
}

//  Sends string as 0MQ string, as multipart non-terminal
inline static int
s_sendmore(void *socket, char *string) {
	int rc;
	zmq_msg_t message;
	zmq_msg_init_size(&message, strlen(string));
	memcpy(zmq_msg_data(&message), string, strlen(string));
	//rc = zmq_send(socket, string, strlen(string), ZMQ_SNDMORE);
	rc = zmq_msg_send(&message, socket, ZMQ_SNDMORE);
	assert(-1 != rc);
	zmq_msg_close(&message);
	return (rc);
}

//  Sends string as 0MQ string, as multipart non-terminal
inline static bool
s_sendmore (zmq::socket_t & socket, const std::string & string) {

    zmq::message_t message(string.size());
    memcpy (message.data(), string.data(), string.size());

	zmq::send_result_t rc = socket.send (message, zmq::send_flags::sndmore);
    return bool(rc);
}

//  Receives all message parts from socket, prints neatly
//
inline static void
s_dump (zmq::socket_t & socket)
{
    std::cout << "----------------------------------------" << std::endl;

    while (1) {
        //  Process all parts of the message
        zmq::message_t message;
		zmq::recv_result_t rc = socket.recv(message);
		if (!rc) {
			std::runtime_error("recv error");
		}

        //  Dump the message as text or binary
        size_t size = message.size();
        std::string data(static_cast<char*>(message.data()), size);

        bool is_text = true;

        size_t char_nbr;
        unsigned char byte;
        for (char_nbr = 0; char_nbr < size; char_nbr++) {
            byte = data [char_nbr];
            if (byte < 32 || byte > 127)
                is_text = false;
        }
        std::cout << "[" << std::setfill('0') << std::setw(3) << size << "]";
        for (char_nbr = 0; char_nbr < size; char_nbr++) {
            if (is_text)
                std::cout << (char)data [char_nbr];
            else
                std::cout << std::setfill('0') << std::setw(2)
                   << std::hex << static_cast<int>(static_cast<unsigned char>(data[char_nbr])); 
        }
        std::cout << std::endl;

        int more = 0;           //  Multipart detection
		more = socket.get(zmq::sockopt::rcvmore);
        if (!more)
            break;              //  Last message part
    }
}

#if (!defined (WIN32))
//  Set simple random printable identity on socket
//  Caution:
//    DO NOT call this version of s_set_id from multiple threads on MS Windows
//    since s_set_id will call rand() on MS Windows. rand(), however, is not 
//    reentrant or thread-safe. See issue #521.
inline std::string
s_set_id (zmq::socket_t & socket)
{
    std::stringstream ss;
    ss << std::hex << std::uppercase
       << std::setw(4) << std::setfill('0') << within (0x10000) << "-"
       << std::setw(4) << std::setfill('0') << within (0x10000);
	socket.set(zmq::sockopt::routing_id, ss.str());
    return ss.str();
}
#else
// Fix #521
inline std::string
s_set_id(zmq::socket_t & socket, intptr_t id)
{
    std::stringstream ss;
    ss << std::hex << std::uppercase
        << std::setw(4) << std::setfill('0') << id;
    socket.setsockopt(ZMQ_IDENTITY, ss.str().c_str(), ss.str().length());
    return ss.str();
}
#endif

//  Report 0MQ version number
//
inline static void
s_version (void)
{
    int major, minor, patch;
    zmq_version (&major, &minor, &patch);
    std::cout << "Current 0MQ version is " << major << "." << minor << "." << patch << std::endl;
}

inline static void
s_version_assert (int want_major, int want_minor)
{
    int major, minor, patch;
    zmq_version (&major, &minor, &patch);
    if (major < want_major
    || (major == want_major && minor < want_minor)) {
        std::cout << "Current 0MQ version is " << major << "." << minor << std::endl;
        std::cout << "Application needs at least " << want_major << "." << want_minor
              << " - cannot continue" << std::endl;
        exit (EXIT_FAILURE);
    }
}

//  Return current system clock as milliseconds
inline static int64_t
s_clock (void)
{
#if (defined (WIN32))
	FILETIME fileTime;
	GetSystemTimeAsFileTime(&fileTime);
	unsigned __int64 largeInt = fileTime.dwHighDateTime;
	largeInt <<= 32;
	largeInt |= fileTime.dwLowDateTime;
	largeInt /= 10000; // FILETIME is in units of 100 nanoseconds
	return (int64_t)largeInt;
#else
    struct timeval tv;
    gettimeofday (&tv, NULL);
    return (int64_t) (tv.tv_sec * 1000 + tv.tv_usec / 1000);
#endif
}

//  Sleep for a number of milliseconds
inline static void
s_sleep (int msecs)
{
#if (defined (WIN32))
    Sleep (msecs);
#else
    struct timespec t;
    t.tv_sec = msecs / 1000;
    t.tv_nsec = (msecs % 1000) * 1000000;
    nanosleep (&t, NULL);
#endif
}

inline static void
s_console (const char *format, ...)
{
    time_t curtime = time (NULL);
    struct tm *loctime = localtime (&curtime);
    char *formatted = new char[20];
    strftime (formatted, 20, "%y-%m-%d %H:%M:%S ", loctime);
    printf ("%s", formatted);
    delete[] formatted;

    va_list argptr;
    va_start (argptr, format);
    vprintf (format, argptr);
    va_end (argptr);
    printf ("\n");
}

//  ---------------------------------------------------------------------
//  Signal handling
//
//  Call s_catch_signals() in your application at startup, and then exit
//  your main loop if s_interrupted is ever 1. Works especially well with
//  zmq_poll.

static int s_interrupted = 0;
inline static void s_signal_handler (int signal_value)
{
    s_interrupted = 1;
}

inline static void s_catch_signals ()
{
#if (!defined(WIN32))
    struct sigaction action;
    action.sa_handler = s_signal_handler;
    action.sa_flags = 0;
    sigemptyset (&action.sa_mask);
    sigaction (SIGINT, &action, NULL);
    sigaction (SIGTERM, &action, NULL);
#endif
}



#endif
