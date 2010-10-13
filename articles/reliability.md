
<A name="toc1-5" title="Sketch for Reliable Request-Reply" />
Sketch for Reliable Request-Reply
=================================

Authors: Pieter Hintjens <ph@imatix.com>

Published Wed 13 October, 2010, 11:38:38.


**<a href="#toc2-14">Overview & Goals</a>**

**<a href="#toc2-19">Architecture</a>**

**<a href="#toc2-67">Client design</a>**

**<a href="#toc2-72">Server design</a>**

**<a href="#toc2-82">Queue design</a>**

**<a href="#toc2-100">Server design</a>**

<A name="toc2-14" title="Overview & Goals" />
## Overview & Goals

This sketch is for a reliable request-reply model between a client and a set of servers.  The assumption is that servers will randomly block and/or die.  The goal is to detect a failure, to handle it, and to recover over time.

<A name="toc2-19" title="Architecture" />
## Architecture

The architecture has these components:

* A set of clients that send requests.
* A set of servers that process requests and send replies.
* A queue device that connects the clients to the servers.

<center>
<img src="http://github.com/imatix/zguide/raw/master/articles/images/reliability_1.png" alt="1">
</center>

<A name="toc2-67" title="Client design" />
## Client design

The client connects to the queue and sends requests in a synchronous fashion.  If it does not get a reply within a certain time (1 second), it reports an error, and retries.  It will retry three times, then exit with a final message.  It uses a REQ socket and zmq_poll.

<A name="toc2-72" title="Server design" />
## Server design

The server connects to the queue and uses the [least-recently used routing][lru] design, i.e. connects a REQ socket to the queue's XREP socket and signals when ready for a new task.  The server will randomly simulate two problems:

1. A crash and restart while processing a request, i.e. close its socket, block for 5 seconds, reopen its socket and restart.
2. A temporary busy wait, i.e. sleep 1 second then continue as normal.

[lru]: http://zguide.zeromq.org/chapter:all#toc46

<A name="toc2-82" title="Queue design" />
## Queue design

The queue binds to a frontend and backend socket and handles requests and replies asynchronously on these using the LRU design.  It manages two lists of servers:

* Servers that are ready for work.
* Servers that are disabled.

Its basic logic is:

* Wait until there is at least one server ready.
* Receive next client request.
* Route request to next ready server and mark that server as busy.
* Wait a short time (10ms) for server response.
* If the server does not respond within this timeout, mark as disabled and resend request to next available server.
* If a reply comes back from a disabled server, discard it.
* When a disabled server signals that it is ready again, move off disabled list.

<A name="toc2-100" title="Server design" />
## Server design

The server connects to the queue's backend socket.  It waits for requests and responds with unique reply messages.  It caches the last request message and reply and if it receives the same request twice, will resend the corresponding reply message.
