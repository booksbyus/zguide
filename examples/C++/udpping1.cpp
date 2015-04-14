/*******************************************************************************
* Author      : James Chapman
* License     : MIT/X11 (http://directory.fsf.org/wiki/License:X11)
* Description : Simple UDP ping example
********************************************************************************/

#include <atomic>
#include <iostream>
#include <string>
#include <thread>

#ifdef _WIN32
#include <WS2tcpip.h>
#else
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif // _WIN32

#include "zmq.hpp"

#ifdef _WIN32
#pragma comment(lib, "Ws2_32.lib")
#endif // _WIN32

#define PING_PORT_NUMBER    9999
#define PING_MSG_SIZE       2
#define PING_INTERVAL       500
#define SOCKET_POLL_TIMEOUT 3000

#define INFO_OUT(MSG)  std::cout << "[INFO]   " << " " << (MSG) << std::endl
#define ERROR_OUT(MSG) std::cerr << "[ERROR]  " << " " << (MSG) << std::endl

#ifndef _WIN32
#define SOCKET int
#define INVALID_SOCKET (SOCKET)(~0)
#define SOCKET_ERROR (SOCKET)(~1)
#define NO_ERROR 0
#endif // _WIN32

std::atomic<bool> g_threadInterupted(false);

/**
* Create a socket and use ZeroMQ to poll.
*/
void listener()
{
    #ifdef _WIN32
    WSADATA wsaData;
    #endif // _WIN32
    int nResult = 0;

    #ifdef _WIN32
    // Initialize Winsock
    nResult = WSAStartup(MAKEWORD(2, 2), &wsaData);
    if (nResult != NO_ERROR)
    {
        ERROR_OUT("zmqListen : WSAStartup failed");
    }
    #endif // _WIN32

    // Create UDP socket
    SOCKET fdSocket;
    fdSocket = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if (fdSocket == INVALID_SOCKET)
    {
        ERROR_OUT("zmqListen : Socket creation failed");
    }

    // Set up the sockaddr structure
    struct sockaddr_in saListen = {0};
    saListen.sin_family = AF_INET;
    saListen.sin_port = htons(PING_PORT_NUMBER);
    saListen.sin_addr.s_addr = htonl(INADDR_ANY);

    // Bind the socket
    nResult = bind(fdSocket, (sockaddr*)&saListen, sizeof(saListen));
    if (nResult != NO_ERROR)
    {
        ERROR_OUT("zmqListen : socket bind failed");
    }

    while (!g_threadInterupted)
    {
        // Poll socket for a message
        zmq::pollitem_t items[] = {{NULL, fdSocket, ZMQ_POLLIN, 0}};
        zmq::poll(&items[0], 1, SOCKET_POLL_TIMEOUT);

        // If we get a message, print the contents
        if (items[0].revents & ZMQ_POLLIN)
        {
            char recvBuf[PING_MSG_SIZE] = {0};
            #ifdef _WIN32
            int saSize = sizeof(struct sockaddr_in);
            #else
            socklen_t saSize = sizeof(struct sockaddr_in);
            #endif // _WIN32
            size_t size = recvfrom(fdSocket, recvBuf, PING_MSG_SIZE + 1, 0, (sockaddr*)&saListen, &saSize);
            {
                std::string ip(inet_ntoa(saListen.sin_addr));
                INFO_OUT("received: " + std::string(recvBuf) + " from " + ip);
            }
        }
    }

    #ifdef _WIN32
    closesocket(fdSocket);
    WSACleanup();
    #endif // _WIN32
}

/**
* Main function starts a listener thread and then sends out 5 broadcast pings
*/
int main()
{
    g_threadInterupted = false;

    // Start listener in a seperate thread
    std::thread listenerThread(listener);

    std::this_thread::sleep_for(std::chrono::milliseconds(1000));

    {
        #ifdef _WIN32
        WSADATA wsaData;
        #endif // _WIN32
        int nResult = 0;
        int nOptOffVal = 0;
        int nOptOnVal = 1;
        int nOptLen = sizeof(int);

        #ifdef _WIN32
        // Initialize Winsock
        nResult = WSAStartup(MAKEWORD(2, 2), &wsaData);
        if (nResult != NO_ERROR)
        {
            ERROR_OUT("broadcast : WSAStartup failed");
        }
        #endif // _WIN32

        // Create UDP socket
        SOCKET fdSocket;
        fdSocket = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
        if (fdSocket == INVALID_SOCKET)
        {
            ERROR_OUT("broadcast : socket creation failed");
        }

        // Ask operating system to let us do broadcasts from socket
        nResult = setsockopt(fdSocket, SOL_SOCKET, SO_BROADCAST, (char *)&nOptOnVal, nOptLen);
        if (nResult != NO_ERROR)
        {
            ERROR_OUT("broadcast : setsockopt SO_BROADCAST failed");
        }

        // Set up the sockaddr structure
        struct sockaddr_in saBroadcast = {0};
        saBroadcast.sin_family = AF_INET;
        saBroadcast.sin_port = htons(PING_PORT_NUMBER);
        saBroadcast.sin_addr.s_addr = htonl(INADDR_BROADCAST);

        // Broadcast 5 beacon messages
        for (int i = 0; i < 5; i++)
        {
            char buffer[PING_MSG_SIZE] = {0};
            strcpy(&buffer[0], "!");
            int bytes = sendto(fdSocket, buffer, PING_MSG_SIZE + 1, 0, (sockaddr*)&saBroadcast, sizeof(struct sockaddr_in));
            if (bytes == SOCKET_ERROR)
            {
                ERROR_OUT("broadcast : sendto failed");
            }

            std::this_thread::sleep_for(std::chrono::milliseconds(PING_INTERVAL));
        }

        #ifdef _WIN32
        closesocket(fdSocket);
        WSACleanup();
        #endif // _WIN32
    }

    std::this_thread::sleep_for(std::chrono::milliseconds(1000));

    g_threadInterupted = true;

    listenerThread.join();

    return 0;
}

