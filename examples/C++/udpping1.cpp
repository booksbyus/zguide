/*******************************************************************************
* Author      : James Chapman
* License     : MIT/X11 (http://directory.fsf.org/wiki/License:X11)
* Description : Simple UDP ping example for Windows (VS2013)
********************************************************************************/

#include <atomic>      // std::atomic
#include <iostream>    // std::cout, std::cerr
#include <string>      // std::string
#include <thread>      // std::thread
#include <WS2tcpip.h>  // Provides sockets et al on Windows

#include "zmq.hpp"     // ZeroMQ cpp bindings

#pragma comment(lib, "Ws2_32.lib") // Tells linker to include Ws2_32.lib

// Define some values
#define PING_PORT_NUMBER    9999
#define PING_MSG_SIZE       2
#define PING_INTERVAL       500
#define SOCKET_POLL_TIMEOUT 3000

// Aesthetic output
#define INFO_OUT(MSG)  std::cout << "[INFO]   " << " " << (MSG) << std::endl
#define ERROR_OUT(MSG) std::cerr << "[ERROR]  " << " " << (MSG) << std::endl

std::atomic<bool> g_threadInterupted = false;

/**
* Create a socket and use ZeroMQ to poll the socket
*/
void listener()
{
    WSADATA wsaData;
    int nResult = 0;
    int nOptOffVal = 0;
    int nOptOnVal = 1;
    int nOptLen = sizeof(int);

    // Initialize Winsock
    nResult = WSAStartup(MAKEWORD(2, 2), &wsaData);
    if (nResult != NO_ERROR)
    {
        ERROR_OUT("zmqListen : WSAStartup failed");
    }

    //  Create UDP socket
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

    //  Bind the socket
    nResult = bind(fdSocket, (sockaddr*)&saListen, sizeof(saListen));
    if (nResult != NO_ERROR)
    {
        ERROR_OUT("zmqListen : socket bind failed");
    }

    while (!g_threadInterupted)
    {
        //  Poll socket for a message
        zmq::pollitem_t items[] = {{NULL, fdSocket, ZMQ_POLLIN, 0}};
        zmq::poll(&items[0], 1, SOCKET_POLL_TIMEOUT);

        //  If we get a message, print the contents
        if (items[0].revents & ZMQ_POLLIN)
        {
            char recvBuf[PING_MSG_SIZE] = {0};
            int saSize = sizeof(struct sockaddr_in);
            size_t size = recvfrom(fdSocket, recvBuf, PING_MSG_SIZE, 0, (sockaddr*)&saListen, &saSize);
            {
                std::string ip(inet_ntoa(saListen.sin_addr));
                INFO_OUT("received: " + std::string(recvBuf) + " from " + ip);
            }
        }
    }

    closesocket(fdSocket);
    WSACleanup();
}

/**
* Main function starts a listener thread and then sends out 5 broadcast pings
*/
int main()
{
    g_threadInterupted = false;

    // Start listener thread
    std::thread listenerThread(listener);

    // Sleep for aesthetic reasons
    Sleep(1000);

    {
        WSADATA wsaData;
        int nResult = 0;
        int nOptOffVal = 0;
        int nOptOnVal = 1;
        int nOptLen = sizeof(int);

        // Initialize Winsock
        nResult = WSAStartup(MAKEWORD(2, 2), &wsaData);
        if (nResult != NO_ERROR)
        {
            ERROR_OUT("broadcast : WSAStartup failed");
        }

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
            int bytes = sendto(fdSocket, buffer, PING_MSG_SIZE, 0, (sockaddr*)&saBroadcast, sizeof(struct sockaddr_in));
            if (bytes == SOCKET_ERROR)
            {
                ERROR_OUT("broadcast : sendto failed");
            }
            Sleep(PING_INTERVAL);
        }

        closesocket(fdSocket);
        WSACleanup();
    }

    Sleep(1000);

    // tell listener thread to stop listening and join
    g_threadInterupted = true;
    listenerThread.join();

    return 0;
}
