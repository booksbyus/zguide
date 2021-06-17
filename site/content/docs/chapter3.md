---
weight: 3
title: '3. Advanced Request-Reply Patterns'
---

# Chapter 3 - Advanced Request-Reply Patterns {#advanced-request-reply}

In [Chapter 2 - Sockets and Patterns](chapter2#sockets-and-patterns) we worked through the basics of using ZeroMQ by developing a series of small applications, each time exploring new aspects of ZeroMQ. We'll continue this approach in this chapter as we explore advanced patterns built on top of ZeroMQ's core request-reply pattern.

We'll cover:

* How the request-reply mechanisms work
* How to combine REQ, REP, DEALER, and ROUTER sockets
* How ROUTER sockets work, in detail
* The load balancing pattern
* Building a simple load balancing message broker
* Designing a high-level API for ZeroMQ
* Building an asynchronous request-reply server
* A detailed inter-broker routing example

## The Request-Reply Mechanisms {#The-Request-Reply-Mechanisms}

We already looked briefly at multipart messages. Let's now look at a major use case, which is *reply message envelopes*. An envelope is a way of safely packaging up data with an address, without touching the data itself. By separating reply addresses into an envelope we make it possible to write general purpose intermediaries such as APIs and proxies that create, read, and remove addresses no matter what the message payload or structure is.

In the request-reply pattern, the envelope holds the return address for replies. It is how a ZeroMQ network with no state can create round-trip request-reply dialogs.

When you use REQ and REP sockets you don't even see envelopes; these sockets deal with them automatically. But for most of the interesting request-reply patterns, you'll want to understand envelopes and particularly ROUTER sockets. We'll work through this step-by-step.

### The Simple Reply Envelope {#The-Simple-Reply-Envelope}

A request-reply exchange consists of a *request* message, and an eventual *reply* message. In the simple request-reply pattern, there's one reply for each request. In more advanced patterns, requests and replies can flow asynchronously. However, the reply envelope always works the same way.

The ZeroMQ reply envelope formally consists of zero or more reply addresses, followed by an empty frame (the envelope delimiter), followed by the message body (zero or more frames). The envelope is created by multiple sockets working together in a chain. We'll break this down.

We'll start by sending "Hello" through a REQ socket. The REQ socket creates the simplest possible reply envelope, which has no addresses, just an empty delimiter frame and the message frame containing the "Hello" string. This is a two-frame message.

{{< textdiagram name="fig26.png" figno="26" title="Request with Minimal Envelope" >}}
          #---#
Frame 1   | 0 |            Empty delimiter frame
          #---#-------#
Frame 2   | 5 | Hello |    Data frame
          #---+-------#
{{< /textdiagram >}}

The REP socket does the matching work: it strips off the envelope, up to and including the delimiter frame, saves the whole envelope, and passes the "Hello" string up the application. Thus our original Hello World example used request-reply envelopes internally, but the application never saw them.

If you spy on the network data flowing between <tt>hwclient</tt> and <tt>hwserver</tt>, this is what you'll see: every request and every reply is in fact two frames, an empty frame and then the body. It doesn't seem to make much sense for a simple REQ-REP dialog. However you'll see the reason when we explore how ROUTER and DEALER handle envelopes.

### The Extended Reply Envelope {#The-Extended-Reply-Envelope}

Now let's extend the REQ-REP pair with a ROUTER-DEALER proxy in the middle and see how this affects the reply envelope. This is the *extended request-reply pattern* we already saw in [Chapter 2 - Sockets and Patterns](chapter2#sockets-and-patterns). We can, in fact, insert any number of proxy steps. The mechanics are the same.

{{< textdiagram name="fig27.png" figno="27" title="Extended Request-Reply Pattern" >}}
#-------#     #-------#
|  REQ  |<--->|  REP  |
#-------#     #-------#


#-------#     #--------+--------#     #-------#
|  REQ  |<--->| ROUTER | DEALER |<--->|  REP  |
#-------#     #--------+--------#     #-------#


#-------#     #--------+--------#     #--------+--------#     #-------#
|  REQ  |<--->| ROUTER | DEALER |<--->| ROUTER | DEALER |<--->|  REP  |
#-------#     #--------+--------#     #--------+--------#     #-------#
{{< /textdiagram >}}

The proxy does this, in pseudo-code:

```
prepare context, frontend and backend sockets
while true:
    poll on both sockets
    if frontend had input:
        read all frames from frontend
        send to backend
    if backend had input:
        read all frames from backend
        send to frontend
```

The ROUTER socket, unlike other sockets, tracks every connection it has, and tells the caller about these. The way it tells the caller is to stick the connection *identity* in front of each message received. An identity, sometimes called an *address*, is just a binary string with no meaning except "this is a unique handle to the connection". Then, when you send a message via a ROUTER socket, you first send an identity frame.

The <tt>[zmq_socket()](http://api.zeromq.org/master:zmq_socket)</tt> man page describes it thus:

> When receiving messages a ZMQ_ROUTER socket shall prepend a message part containing the identity of the originating peer to the message before passing it to the application. Messages received are fair-queued from among all connected peers. When sending messages a ZMQ_ROUTER socket shall remove the first part of the message and use it to determine the identity of the peer the message shall be routed to.

As a historical note, ZeroMQ v2.2 and earlier use UUIDs as identities. ZeroMQ v3.0 and later generate a 5 byte identity by default (0 + a random 32bit integer). There's some impact on network performance, but only when you use multiple proxy hops, which is rare. Mostly the change was to simplify building <tt>libzmq</tt> by removing the dependency on a UUID library.

Identities are a difficult concept to understand, but it's essential if you want to become a ZeroMQ expert. The ROUTER socket *invents* a random identity for each connection with which it works. If there are three REQ sockets connected to a ROUTER socket, it will invent three random identities, one for each REQ socket.

So if we continue our worked example, let's say the REQ socket has a 3-byte identity <tt>ABC</tt>. Internally, this means the ROUTER socket keeps a hash table where it can search for <tt>ABC</tt> and find the TCP connection for the REQ socket.

When we receive the message off the ROUTER socket, we get three frames.

{{< textdiagram name="fig28.png" figno="28" title="Request with One Address" >}}
          #---+-----#
Frame 1   | 3 | ABC |      Identity of connection
          #---#-----#
Frame 2   | 0 |            Empty delimiter frame
          #---#-------#
Frame 3   | 5 | Hello |    Data frame
          #---+-------#
{{< /textdiagram >}}

The core of the proxy loop is "read from one socket, write to the other", so we literally send these three frames out on the DEALER socket. If you now sniffed the network traffic, you would see these three frames flying from the DEALER socket to the REP socket. The REP socket does as before, strips off the whole envelope including the new reply address, and once again delivers the "Hello" to the caller.

Incidentally the REP socket can only deal with one request-reply exchange at a time, which is why if you try to read multiple requests or send multiple replies without sticking to a strict recv-send cycle, it gives an error.

You should now be able to visualize the return path. When <tt>hwserver</tt> sends "World" back, the REP socket wraps that with the envelope it saved, and sends a three-frame reply message across the wire to the DEALER socket.

{{< textdiagram name="fig29.png" figno="29" title="Reply with one Address" >}}
          #---+-----#
Frame 1   | 3 | ABC |      Identity of connection
          #---#-----#
Frame 2   | 0 |            Empty delimiter frame
          #---#-------#
Frame 3   | 5 | World |    Data frame
          #---+-------#
{{< /textdiagram >}}

Now the DEALER reads these three frames, and sends all three out via the ROUTER socket. The ROUTER takes the first frame for the message, which is the <tt>ABC</tt> identity, and looks up the connection for this. If it finds that, it then pumps the next two frames out onto the wire.

{{< textdiagram name="fig30.png" figno="30" title="Reply with Minimal Envelope" >}}
          #---#
Frame 1   | 0 |            Empty delimiter frame
          #---#-------#
Frame 2   | 5 | World |    Data frame
          #---+-------#
{{< /textdiagram >}}

The REQ socket picks this message up, and checks that the first frame is the empty delimiter, which it is. The REQ socket discards that frame and passes "World" to the calling application, which prints it out to the amazement of the younger us looking at ZeroMQ for the first time.

### What's This Good For? {#What-s-This-Good-For}

To be honest, the use cases for strict request-reply or extended request-reply are somewhat limited. For one thing, there's no easy way to recover from common failures like the server crashing due to buggy application code. We'll see more about this in [Chapter 4 - Reliable Request-Reply Patterns](chapter4#reliable-request-reply). However once you grasp the way these four sockets deal with envelopes, and how they talk to each other, you can do very useful things. We saw how ROUTER uses the reply envelope to decide which client REQ socket to route a reply back to. Now let's express this another way:

* Each time ROUTER gives you a message, it tells you what peer that came from, as an identity.
* You can use this with a hash table (with the identity as key) to track new peers as they arrive.
* ROUTER will route messages asynchronously to any peer connected to it, if you prefix the identity as the first frame of the message.

ROUTER sockets don't care about the whole envelope. They don't know anything about the empty delimiter. All they care about is that one identity frame that lets them figure out which connection to send a message to.

### Recap of Request-Reply Sockets {#Recap-of-Request-Reply-Sockets}

Let's recap this:

* The REQ socket sends, to the network, an empty delimiter frame in front of the message data. REQ sockets are synchronous. REQ sockets always send one request and then wait for one reply. REQ sockets talk to one peer at a time. If you connect a REQ socket to multiple peers, requests are distributed to and replies expected from each peer one turn at a time.

* The REP socket reads and saves all identity frames up to and including the empty delimiter, then passes the following frame or frames to the caller. REP sockets are synchronous and talk to one peer at a time. If you connect a REP socket to multiple peers, requests are read from peers in fair fashion, and replies are always sent to the same peer that made the last request.

* The DEALER socket is oblivious to the reply envelope and handles this like any multipart message. DEALER sockets are asynchronous and like PUSH and PULL combined. They distribute sent messages among all connections, and fair-queue received messages from all connections.

* The ROUTER socket is oblivious to the reply envelope, like DEALER. It creates identities for its connections, and passes these identities to the caller as a first frame in any received message. Conversely, when the caller sends a message, it uses the first message frame as an identity to look up the connection to send to. ROUTERS are asynchronous.

## Request-Reply Combinations {#Request-Reply-Combinations}

We have four request-reply sockets, each with a certain behavior. We've seen how they connect in simple and extended request-reply patterns. But these sockets are building blocks that you can use to solve many problems.

These are the legal combinations:

* REQ to REP
* DEALER to REP
* REQ to ROUTER
* DEALER to ROUTER
* DEALER to DEALER
* ROUTER to ROUTER

And these combinations are invalid (and I'll explain why):

* REQ to REQ
* REQ to DEALER
* REP to REP
* REP to ROUTER

Here are some tips for remembering the semantics. DEALER is like an asynchronous REQ socket, and ROUTER is like an asynchronous REP socket. Where we use a REQ socket, we can use a DEALER; we just have to read and write the envelope ourselves. Where we use a REP socket, we can stick a ROUTER; we just need to manage the identities ourselves.

Think of REQ and DEALER sockets as "clients" and REP and ROUTER sockets as "servers". Mostly, you'll want to bind REP and ROUTER sockets, and connect REQ and DEALER sockets to them. It's not always going to be this simple, but it is a clean and memorable place to start.

### The REQ to REP Combination {#The-REQ-to-REP-Combination}

We've already covered a REQ client talking to a REP server but let's take one aspect: the REQ client *must* initiate the message flow. A REP server cannot talk to a REQ client that hasn't first sent it a request. Technically, it's not even possible, and the API also returns an <tt>EFSM</tt> error if you try it.

### The DEALER to REP Combination {#The-DEALER-to-REP-Combination}

Now, let's replace the REQ client with a DEALER. This gives us an asynchronous client that can talk to multiple REP servers. If we rewrote the "Hello World" client using DEALER, we'd be able to send off any number of "Hello" requests without waiting for replies.

When we use a DEALER to talk to a REP socket, we *must* accurately emulate the envelope that the REQ socket would have sent, or the REP socket will discard the message as invalid. So, to send a message, we:

* Send an empty message frame with the MORE flag set; then
* Send the message body.

And when we receive a message, we:

* Receive the first frame and if it's not empty, discard the whole message;
* Receive the next frame and pass that to the application.

### The REQ to ROUTER Combination {#The-REQ-to-ROUTER-Combination}

In the same way that we can replace REQ with DEALER, we can replace REP with ROUTER. This gives us an asynchronous server that can talk to multiple REQ clients at the same time. If we rewrote the "Hello World" server using ROUTER, we'd be able to process any number of "Hello" requests in parallel. We saw this in the [Chapter 2 - Sockets and Patterns](chapter2#sockets-and-patterns) <tt>mtserver</tt> example.

We can use ROUTER in two distinct ways:

* As a proxy that switches messages between frontend and backend sockets.
* As an application that reads the message and acts on it.

In the first case, the ROUTER simply reads all frames, including the artificial identity frame, and passes them on blindly. In the second case the ROUTER *must* know the format of the reply envelope it's being sent. As the other peer is a REQ socket, the ROUTER gets the identity frame, an empty frame, and then the data frame.

### The DEALER to ROUTER Combination {#The-DEALER-to-ROUTER-Combination}

Now we can switch out both REQ and REP with DEALER and ROUTER to get the most powerful socket combination, which is DEALER talking to ROUTER. It gives us asynchronous clients talking to asynchronous servers, where both sides have full control over the message formats.

Because both DEALER and ROUTER can work with arbitrary message formats, if you hope to use these safely, you have to become a little bit of a protocol designer. At the very least you must decide whether you wish to emulate the REQ/REP reply envelope. It depends on whether you actually need to send replies or not.

### The DEALER to DEALER Combination {#The-DEALER-to-DEALER-Combination}

You can swap a REP with a ROUTER, but you can also swap a REP with a DEALER, if the DEALER is talking to one and only one peer.

When you replace a REP with a DEALER, your worker can suddenly go full asynchronous, sending any number of replies back. The cost is that you have to manage the reply envelopes yourself, and get them right, or nothing at all will work. We'll see a worked example later. Let's just say for now that DEALER to DEALER is one of the trickier patterns to get right, and happily it's rare that we need it.

### The ROUTER to ROUTER Combination {#The-ROUTER-to-ROUTER-Combination}

This sounds perfect for N-to-N connections, but it's the most difficult combination to use. You should avoid it until you are well advanced with ZeroMQ. We'll see one example it in the Freelance pattern in [Chapter 4 - Reliable Request-Reply Patterns](chapter4#reliable-request-reply), and an alternative DEALER to ROUTER design for peer-to-peer work in [Chapter 8 - A Framework for Distributed Computing](chapter8#moving-pieces).

### Invalid Combinations {#Invalid-Combinations}

Mostly, trying to connect clients to clients, or servers to servers is a bad idea and won't work. However, rather than give general vague warnings, I'll explain in detail:

* REQ to REQ: both sides want to start by sending messages to each other, and this could only work if you timed things so that both peers exchanged messages at the same time. It hurts my brain to even think about it.

* REQ to DEALER: you could in theory do this, but it would break if you added a second REQ because DEALER has no way of sending a reply to the original peer. Thus the REQ socket would get confused, and/or return messages meant for another client.

* REP to REP: both sides would wait for the other to send the first message.

* REP to ROUTER: the ROUTER socket can in theory initiate the dialog and send a properly-formatted request, if it knows the REP socket has connected *and* it knows the identity of that connection. It's messy and adds nothing over DEALER to ROUTER.

The common thread in this valid versus invalid breakdown is that a ZeroMQ socket connection is always biased towards one peer that binds to an endpoint, and another that connects to that. Further, that which side binds and which side connects is not arbitrary, but follows natural patterns. The side which we expect to "be there" binds: it'll be a server, a broker, a publisher, a collector. The side that "comes and goes" connects: it'll be clients and workers. Remembering this will help you design better ZeroMQ architectures.

## Exploring ROUTER Sockets {#Exploring-ROUTER-Sockets}

Let's look at ROUTER sockets a little closer. We've already seen how they work by routing individual messages to specific connections. I'll explain in more detail how we identify those connections, and what a ROUTER socket does when it can't send a message.

### Identities and Addresses {#Identities-and-Addresses}

The *identity* concept in ZeroMQ refers specifically to ROUTER sockets and how they identify the connections they have to other sockets. More broadly, identities are used as addresses in the reply envelope. In most cases, the identity is arbitrary and local to the ROUTER socket: it's a lookup key in a hash table. Independently, a peer can have an address that is physical (a network endpoint like "tcp://192.168.55.117:5670") or logical (a UUID or email address or other unique key).

An application that uses a ROUTER socket to talk to specific peers can convert a logical address to an identity if it has built the necessary hash table. Because ROUTER sockets only announce the identity of a connection (to a specific peer) when that peer sends a message, you can only really reply to a message, not spontaneously talk to a peer.

This is true even if you flip the rules and make the ROUTER connect to the peer rather than wait for the peer to connect to the ROUTER. However you can force the ROUTER socket to use a logical address in place of its identity. The <tt>zmq_setsockopt</tt> reference page calls this *setting the socket identity*. It works as follows:

* The peer application sets the <tt>ZMQ_IDENTITY</tt> option of its peer socket (DEALER or REQ) *before* binding or connecting.
* Usually the peer then connects to the already-bound ROUTER socket. But the ROUTER can also connect to the peer.
* At connection time, the peer socket tells the router socket, "please use this identity for this connection".
* If the peer socket doesn't say that, the router generates its usual arbitrary random identity for the connection.
* The ROUTER socket now provides this logical address to the application as a prefix identity frame for any messages coming in from that peer.
* The ROUTER also expects the logical address as the prefix identity frame for any outgoing messages.

Here is a simple example of two peers that connect to a ROUTER socket, one that imposes a logical address "PEER2":

{{< examples name="identity" title="Identity check" >}}

Here is what the program prints:

```
----------------------------------------
[005] 006B8B4567
[000]
[039] ROUTER uses a generated 5 byte identity
----------------------------------------
[005] PEER2
[000]
[038] ROUTER uses REQ's socket identity
```

### ROUTER Error Handling {#ROUTER-Error-Handling}

ROUTER sockets do have a somewhat brutal way of dealing with messages they can't send anywhere: they drop them silently. It's an attitude that makes sense in working code, but it makes debugging hard. The "send identity as first frame" approach is tricky enough that we often get this wrong when we're learning, and the ROUTER's stony silence when we mess up isn't very constructive.

Since ZeroMQ v3.2 there's a socket option you can set to catch this error: <tt>ZMQ_ROUTER_MANDATORY</tt>. Set that on the ROUTER socket and then when you provide an unroutable identity on a send call, the socket will signal an <tt>EHOSTUNREACH</tt> error.

## The Load Balancing Pattern {#The-Load-Balancing-Pattern}

Now let's look at some code. We'll see how to connect a ROUTER socket to a REQ socket, and then to a DEALER socket. These two examples follow the same logic, which is a *load balancing* pattern. This pattern is our first exposure to using the ROUTER socket for deliberate routing, rather than simply acting as a reply channel.

The load balancing pattern is very common and we'll see it several times in this book. It solves the main problem with simple round robin routing (as PUSH and DEALER offer) which is that round robin becomes inefficient if tasks do not all roughly take the same time.

It's the post office analogy. If you have one queue per counter, and you have some people buying stamps (a fast, simple transaction), and some people opening new accounts (a very slow transaction), then you will find stamp buyers getting unfairly stuck in queues. Just as in a post office, if your messaging architecture is unfair, people will get annoyed.

The solution in the post office is to create a single queue so that even if one or two counters get stuck with slow work, other counters will continue to serve clients on a first-come, first-serve basis.

One reason PUSH and DEALER use the simplistic approach is sheer performance. If you arrive in any major US airport, you'll find long queues of people waiting at immigration. The border patrol officials will send people in advance to queue up at each counter, rather than using a single queue. Having people walk fifty yards in advance saves a minute or two per passenger. And because every passport check takes roughly the same time, it's more or less fair. This is the strategy for PUSH and DEALER: send work loads ahead of time so that there is less travel distance.

This is a recurring theme with ZeroMQ: the world's problems are diverse and you can benefit from solving different problems each in the right way. The airport isn't the post office and one size fits no one, really well.

Let's return to the scenario of a worker (DEALER or REQ) connected to a broker (ROUTER). The broker has to know when the worker is ready, and keep a list of workers so that it can take the *least recently used* worker each time.

The solution is really simple, in fact: workers send a "ready" message when they start, and after they finish each task. The broker reads these messages one-by-one. Each time it reads a message, it is from the last used worker. And because we're using a ROUTER socket, we get an identity that we can then use to send a task back to the worker.

It's a twist on request-reply because the task is sent with the reply, and any response for the task is sent as a new request. The following code examples should make it clearer.

### ROUTER Broker and REQ Workers {#ROUTER-Broker-and-REQ-Workers}

Here is an example of the load balancing pattern using a ROUTER broker talking to a set of REQ workers:

{{< examples name="rtreq" title="ROUTER-to-REQ" >}}

The example runs for five seconds and then each worker prints how many tasks they handled. If the routing worked, we'd expect a fair distribution of work:

```
Completed: 20 tasks
Completed: 18 tasks
Completed: 21 tasks
Completed: 23 tasks
Completed: 19 tasks
Completed: 21 tasks
Completed: 17 tasks
Completed: 17 tasks
Completed: 25 tasks
Completed: 19 tasks
```

To talk to the workers in this example, we have to create a REQ-friendly envelope consisting of an identity plus an empty envelope delimiter frame.

{{< textdiagram name="fig31.png" figno="31" title="Routing Envelope for REQ" >}}
          #---+-------#
Frame 1   | n | ...   |     Identity of connection
          #---#-------#
Frame 2   | 0 |             Empty delimiter frame
          #---#--------#
Frame 3   | n | ...    |    Data frame
          #---+--------#
{{< /textdiagram >}}

### ROUTER Broker and DEALER Workers {#ROUTER-Broker-and-DEALER-Workers}

Anywhere you can use REQ, you can use DEALER. There are two specific differences:

* The REQ socket always sends an empty delimiter frame before any data frames; the DEALER does not.
* The REQ socket will send only one message before it receives a reply; the DEALER is fully asynchronous.

The synchronous versus asynchronous behavior has no effect on our example because we're doing strict request-reply. It is more relevant when we address recovering from failures, which we'll come to in [Chapter 4 - Reliable Request-Reply Patterns](chapter4#reliable-request-reply).

Now let's look at exactly the same example but with the REQ socket replaced by a DEALER socket:

{{< examples name="rtdealer" title="ROUTER-to-DEALER" >}}

The code is almost identical except that the worker uses a DEALER socket, and reads and writes that empty frame before the data frame. This is the approach I use when I want to keep compatibility with REQ workers.

However, remember the reason for that empty delimiter frame: it's to allow multihop extended requests that terminate in a REP socket, which uses that delimiter to split off the reply envelope so it can hand the data frames to its application.

If we never need to pass the message along to a REP socket, we can simply drop the empty delimiter frame at both sides, which makes things simpler. This is usually the design I use for pure DEALER to ROUTER protocols.

### A Load Balancing Message Broker {#A-Load-Balancing-Message-Broker}

The previous example is half-complete. It can manage a set of workers with dummy requests and replies, but it has no way to talk to clients. If we add a second *frontend* ROUTER socket that accepts client requests, and turn our example into a proxy that can switch messages from frontend to backend, we get a useful and reusable tiny load balancing message broker.

{{< textdiagram name="fig32.png" figno="32" title="Load Balancing Broker" >}}
#--------#  #--------#  #--------#
| Client |  | Client |  | Client |
+--------+  +--------+  +--------+
|  REQ   |  |  REQ   |  |  REQ   |
'---+----'  '---+----'  '---+----'
    |           |           |
    '-----------+-----------'
                |
            .---+----.
            | ROUTER |  Frontend
            +--------+
            | Proxy  |  Load balancer
            +--------+
            | ROUTER |  Backend
            '---+----'
                |
    .-----------+-----------.
    |           |           |
.---+----.  .---+----.  .---+----.
|  REQ   |  |  REQ   |  |  REQ   |
+--------+  +--------+  +--------+
| Worker |  | Worker |  | Worker |
#--------#  #--------#  #--------#
{{< /textdiagram >}}

This broker does the following:

* Accepts connections from a set of clients.
* Accepts connections from a set of workers.
* Accepts requests from clients and holds these in a single queue.
* Sends these requests to workers using the load balancing pattern.
* Receives replies back from workers.
* Sends these replies back to the original requesting client.

The broker code is fairly long, but worth understanding:

{{< examples name="lbbroker" title="Load balancing broker" >}}

The difficult part of this program is (a) the envelopes that each socket reads and writes, and (b) the load balancing algorithm. We'll take these in turn, starting with the message envelope formats.

Let's walk through a full request-reply chain from client to worker and back. In this code we set the identity of client and worker sockets to make it easier to trace the message frames. In reality, we'd allow the ROUTER sockets to invent identities for connections. Let's assume the client's identity is "CLIENT" and the worker's identity is "WORKER". The client application sends a single frame containing "Hello".

{{< textdiagram name="fig33.png" figno="33" title="Message that Client Sends" >}}
          #---+-------#
Frame 1   | 5 | Hello |   Data frame
          #---+-------#
{{< /textdiagram >}}

Because the REQ socket adds its empty delimiter frame and the ROUTER socket adds its connection identity, the proxy reads off the frontend ROUTER socket the client address, empty delimiter frame, and the data part.

{{< textdiagram name="fig34.png" figno="34" title="Message Coming in on Frontend" >}}
          #---+--------#
Frame 1   | 6 | CLIENT |   Client address
          #---#--------#
Frame 2   | 0 |            Empty delimiter frame
          #---#-------#
Frame 3   | 5 | Hello |    Data frame
          #---+-------#
{{< /textdiagram >}}

The broker sends this to the worker, prefixed by the address of the chosen worker, plus an additional empty part to keep the REQ at the other end happy.

{{< textdiagram name="fig35.png" figno="35" title="Message Sent to Backend" >}}
          #---+--------#
Frame 1   | 6 | WORKER |   Address of worker
          #---#--------#
Frame 2   | 0 |            Empty delimiter frame
          #---#--------#
Frame 3   | 6 | CLIENT |   Identity of client
          #---#--------#
Frame 4   | 0 |            Empty delimiter frame
          #---#-------#
Frame 5   | 5 | Hello |    Data frame
          #---+-------#
{{< /textdiagram >}}

This complex envelope stack gets chewed up first by the backend ROUTER socket, which removes the first frame. Then the REQ socket in the worker removes the empty part, and provides the rest to the worker application.

{{< textdiagram name="fig36.png" figno="36" title="Message Delivered to Worker" >}}
          #---+--------#
Frame 1   | 6 | CLIENT |   Identity of client
          #---#--------#
Frame 2   | 0 |            Empty delimiter frame
          #---#-------#
Frame 3   | 5 | Hello |    Data frame
          #---+-------#
{{< /textdiagram >}}

The worker has to save the envelope (which is all the parts up to and including the empty message frame) and then it can do what's needed with the data part. Note that a REP socket would do this automatically, but we're using the REQ-ROUTER pattern so that we can get proper load balancing.

On the return path, the messages are the same as when they come in, i.e., the backend socket gives the broker a message in five parts, and the broker sends the frontend socket a message in three parts, and the client gets a message in one part.

Now let's look at the load balancing algorithm. It requires that both clients and workers use REQ sockets, and that workers correctly store and replay the envelope on messages they get. The algorithm is:

* Create a pollset that always polls the backend, and polls the frontend only if there are one or more workers available.

* Poll for activity with infinite timeout.

* If there is activity on the backend, we either have a "ready" message or a reply for a client. In either case, we store the worker address (the first part) on our worker queue, and if the rest is a client reply, we send it back to that client via the frontend.

* If there is activity on the frontend, we take the client request, pop the next worker (which is the last used), and send the request to the backend. This means sending the worker address, empty part, and then the three parts of the client request.

You should now see that you can reuse and extend the load balancing algorithm with variations based on the information the worker provides in its initial "ready" message. For example, workers might start up and do a performance self test, then tell the broker how fast they are. The broker can then choose the fastest available worker rather than the oldest.

## A High-Level API for ZeroMQ {#A-High-Level-API-for-ZeroMQ}

We're going to push request-reply onto the stack and open a different area, which is the ZeroMQ API itself. There's a reason for this detour: as we write more complex examples, the low-level ZeroMQ API starts to look increasingly clumsy. Look at the core of the worker thread from our load balancing broker:

{{< fragment name="lbreader" >}}
while (true) {
    //  Get one address frame and empty delimiter
    char *address = s_recv (worker);
    char *empty = s_recv (worker);
    assert (*empty == 0);
    free (empty);

    //  Get request, send reply
    char *request = s_recv (worker);
    printf ("Worker: %s\n", request);
    free (request);

    s_sendmore (worker, address);
    s_sendmore (worker, "");
    s_send     (worker, "OK");
    free (address);
}
{{< /fragment >}}

That code isn't even reusable because it can only handle one reply address in the envelope, and it already does some wrapping around the ZeroMQ API. If we used the <tt>libzmq</tt> simple message API this is what we'd have to write:

{{< fragment name="lowreader" >}}
while (true) {
    //  Get one address frame and empty delimiter
    char address [255];
    int address_size = zmq_recv (worker, address, 255, 0);
    if (address_size == -1)
        break;

    char empty [1];
    int empty_size = zmq_recv (worker, empty, 1, 0);
    assert (empty_size <= 0);
    if (empty_size == -1)
        break;

    //  Get request, send reply
    char request [256];
    int request_size = zmq_recv (worker, request, 255, 0);
    if (request_size == -1)
        return NULL;
    request [request_size] = 0;
    printf ("Worker: %s\n", request);

    zmq_send (worker, address, address_size, ZMQ_SNDMORE);
    zmq_send (worker, empty, 0, ZMQ_SNDMORE);
    zmq_send (worker, "OK", 2, 0);
}
{{< /fragment >}}

And when code is too long to write quickly, it's also too long to understand. Up until now, I've stuck to the native API because, as ZeroMQ users, we need to know that intimately. But when it gets in our way, we have to treat it as a problem to solve.

We can't of course just change the ZeroMQ API, which is a documented public contract on which thousands of people agree and depend. Instead, we construct a higher-level API on top based on our experience so far, and most specifically, our experience from writing more complex request-reply patterns.

What we want is an API that lets us receive and send an entire message in one shot, including the reply envelope with any number of reply addresses. One that lets us do what we want with the absolute least lines of code.

Making a good message API is fairly difficult. We have a problem of terminology: ZeroMQ uses "message" to describe both multipart messages, and individual message frames. We have a problem of expectations: sometimes it's natural to see message content as printable string data, sometimes as binary blobs. And we have technical challenges, especially if we want to avoid copying data around too much.

The challenge of making a good API affects all languages, though my specific use case is C. Whatever language you use, think about how you could contribute to your language binding to make it as good (or better) than the C binding I'm going to describe.

### Features of a Higher-Level API {#Features-of-a-Higher-Level-API}

My solution is to use three fairly natural and obvious concepts: *string* (already the basis for our <tt>s_send</tt> and <tt>s_recv</tt>) helpers, *frame* (a message frame), and *message* (a list of one or more frames). Here is the worker code, rewritten onto an API using these concepts:

{{< fragment name="highreader" >}}
while (true) {
    zmsg_t *msg = zmsg_recv (worker);
    zframe_reset (zmsg_last (msg), "OK", 2);
    zmsg_send (&msg, worker);
}
{{< /fragment >}}

Cutting the amount of code we need to read and write complex messages is great: the results are easy to read and understand. Let's continue this process for other aspects of working with ZeroMQ. Here's a wish list of things I'd like in a higher-level API, based on my experience with ZeroMQ so far:

* *Automatic handling of sockets.* I find it cumbersome to have to close sockets manually, and to have to explicitly define the linger timeout in some (but not all) cases. It'd be great to have a way to close sockets automatically when I close the context.

* *Portable thread management.* Every nontrivial ZeroMQ application uses threads, but POSIX threads aren't portable. So a decent high-level API should hide this under a portable layer.

* *Piping from parent to child threads.* It's a recurrent problem: how to signal between parent and child threads. Our API should provide a ZeroMQ message pipe (using PAIR sockets and <tt>inproc</tt> automatically.

* *Portable clocks.* Even getting the time to a millisecond resolution, or sleeping for some milliseconds, is not portable. Realistic ZeroMQ applications need portable clocks, so our API should provide them.

* *A reactor to replace <tt>[zmq_poll()](http://api.zeromq.org/master:zmq_poll)</tt>.* The poll loop is simple, but clumsy. Writing a lot of these, we end up doing the same work over and over: calculating timers, and calling code when sockets are ready. A simple reactor with socket readers and timers would save a lot of repeated work.

* *Proper handling of Ctrl-C.* We already saw how to catch an interrupt. It would be useful if this happened in all applications.

### The CZMQ High-Level API {#The-CZMQ-High-Level-API}

Turning this wish list into reality for the C language gives us [CZMQ](http://zero.mq/c), a ZeroMQ language binding for C. This high-level binding, in fact, developed out of earlier versions of the examples. It combines nicer semantics for working with ZeroMQ with some portability layers, and (importantly for C, but less for other languages) containers like hashes and lists. CZMQ also uses an elegant object model that leads to frankly lovely code.

Here is the load balancing broker rewritten to use a higher-level API (CZMQ for the C case):

{{< examples name="lbbroker2" title="Load balancing broker using high-level API" >}}

One thing CZMQ provides is clean interrupt handling. This means that Ctrl-C will cause any blocking ZeroMQ call to exit with a return code -1 and errno set to <tt>EINTR</tt>. The high-level recv methods will return NULL in such cases. So, you can cleanly exit a loop like this:

{{< fragment name="interrupt" >}}
while (true) {
    zstr_send (client, "Hello");
    char *reply = zstr_recv (client);
    if (!reply)
        break;              //  Interrupted
    printf ("Client: %s\n", reply);
    free (reply);
    sleep (1);
}
{{< /fragment >}}

Or, if you're calling <tt>[zmq_poll()](http://api.zeromq.org/master:zmq_poll)</tt>, test on the return code:

{{< fragment name="polling" >}}
if (zmq_poll (items, 2, 1000 * 1000) == -1)
    break;              //  Interrupted
{{< /fragment >}}

The previous example still uses <tt>[zmq_poll()](http://api.zeromq.org/master:zmq_poll)</tt>. So how about reactors? The CZMQ <tt>zloop</tt> reactor is simple but functional. It lets you:

* Set a reader on any socket, i.e., code that is called whenever the socket has input.
* Cancel a reader on a socket.
* Set a timer that goes off once or multiple times at specific intervals.
* Cancel a timer.

<tt>zloop</tt> of course uses <tt>[zmq_poll()](http://api.zeromq.org/master:zmq_poll)</tt> internally. It rebuilds its poll set each time you add or remove readers, and it calculates the poll timeout to match the next timer. Then, it calls the reader and timer handlers for each socket and timer that need attention.

When we use a reactor pattern, our code turns inside out. The main logic looks like this:

{{< fragment name="reactor" >}}
zloop_t *reactor = zloop_new ();
zloop_reader (reactor, self->backend, s_handle_backend, self);
zloop_start (reactor);
zloop_destroy (&reactor);
{{< /fragment >}}

The actual handling of messages sits inside dedicated functions or methods. You may not like the style--it's a matter of taste. What it does help with is mixing timers and socket activity. In the rest of this text, we'll use <tt>[zmq_poll()](http://api.zeromq.org/master:zmq_poll)</tt> in simpler cases, and <tt>zloop</tt> in more complex examples.

Here is the load balancing broker rewritten once again, this time to use <tt>zloop</tt>:

{{< examples name="lbbroker3" title="Load balancing broker using zloop" >}}

Getting applications to properly shut down when you send them Ctrl-C can be tricky. If you use the <tt>zctx</tt> class it'll automatically set up signal handling, but your code still has to cooperate. You must break any loop if <tt>zmq_poll</tt> returns -1 or if any of the <tt>zstr_recv</tt>, <tt>zframe_recv</tt>, or <tt>zmsg_recv</tt> methods return NULL. If you have nested loops, it can be useful to make the outer ones conditional on <tt>!zctx_interrupted</tt>.

If you're using child threads, they won't receive the interrupt. To tell them to shutdown, you can either:

* Destroy the context, if they are sharing the same context, in which case any blocking calls they are waiting on will end with ETERM.
* Send them shutdown messages, if they are using their own contexts. For this you'll need some socket plumbing.

## The Asynchronous Client/Server Pattern {#The-Asynchronous-Client-Server-Pattern}

In the ROUTER to DEALER example, we saw a 1-to-N use case where one server talks asynchronously to multiple workers. We can turn this upside down to get a very useful N-to-1 architecture where various clients talk to a single server, and do this asynchronously.

{{< textdiagram name="fig37.png" figno="37" title="Asynchronous Client/Server" >}}
#----------#   #----------#
|  Client  |   |  Client  |
+----------+   +----------+
|  DEALER  |   |  DEALER  |
'----------'   '----------'
      ^              ^
      |              |
      '------+-------'
             |
             v
      .-------------.
      |   ROUTER    |
      +-------------+
      |   Server    |
      #-------------#
{{< /textdiagram >}}

Here's how it works:

* Clients connect to the server and send requests.
* For each request, the server sends 0 or more replies.
* Clients can send multiple requests without waiting for a reply.
* Servers can send multiple replies without waiting for new requests.

Here's code that shows how this works:

{{< examples name="asyncsrv" title="Asynchronous client/server" >}}

The example runs in one process, with multiple threads simulating a real multiprocess architecture. When you run the example, you'll see three clients (each with a random ID), printing out the replies they get from the server. Look carefully and you'll see each client task gets 0 or more replies per request.

Some comments on this code:

* The clients send a request once per second, and get zero or more replies back. To make this work using <tt>[zmq_poll()](http://api.zeromq.org/master:zmq_poll)</tt>, we can't simply poll with a 1-second timeout, or we'd end up sending a new request only one second *after we received the last reply*. So we poll at a high frequency (100 times at 1/100th of a second per poll), which is approximately accurate.

* The server uses a pool of worker threads, each processing one request synchronously. It connects these to its frontend socket using an internal queue. It connects the frontend and backend sockets using a <tt>[zmq_proxy()](http://api.zeromq.org/master:zmq_proxy)</tt> call.

{{< textdiagram name="fig38.png" figno="38" title="Detail of Asynchronous Server" >}}
   #---------#   #---------#   #---------#
   | Client  |   | Client  |   | Client  |
   +---------+   +---------+   +---------+
   | DEALER  |   | DEALER  |   | DEALER  |
   '---------'   '---------'   '---------'
     connect       connect       connect
        |             |             |
        '-------------+-------------'
                      |
.-------------------- | --------------------.
:                     v                     :
:                   bind                    :
:               .-----------.               :
:               |  ROUTER   |               :
:               +-----------+               :
:               |  Server   |               :
:               +-----------+               :
:               |  DEALER   |               :
:               '-----------'               :
:                   bind                    :
:                     |                     :
:       .-------------+-------------.       :
:       |             |             |       :
:       v             v             v       :
:    connect       connect       connect    :
:  .---------.   .---------.   .---------.  :
:  | DEALER  |   | DEALER  |   | DEALER  |  :
:  +---------+   +---------+   +---------+  :
:  | Worker  |   | Worker  |   | Worker  |  :
:  #---------#   #---------#   #---------#  :
'-------------------------------------------'
{{< /textdiagram >}}

Note that we're doing DEALER to ROUTER dialog between client and server, but internally between the server main thread and workers, we're doing DEALER to DEALER. If the workers were strictly synchronous, we'd use REP. However, because we want to send multiple replies, we need an async socket. We do *not* want to route replies, they always go to the single server thread that sent us the request.

Let's think about the routing envelope. The client sends a message consisting of a single frame. The server thread receives a two-frame message (original message prefixed by client identity). We send these two frames on to the worker, which treats it as a normal reply envelope, returns that to us as a two frame message. We then use the first frame as an identity to route the second frame back to the client as a reply.

It looks something like this:

```
     client          server       frontend       worker
   [ DEALER ]<---->[ ROUTER <----> DEALER <----> DEALER ]
             1 part         2 parts       2 parts
```

Now for the sockets: we could use the load balancing ROUTER to DEALER pattern to talk to workers, but it's extra work. In this case, a DEALER to DEALER pattern is probably fine: the trade-off is lower latency for each request, but higher risk of unbalanced work distribution. Simplicity wins in this case.

When you build servers that maintain stateful conversations with clients, you will run into a classic problem. If the server keeps some state per client, and clients keep coming and going, eventually it will run out of resources. Even if the same clients keep connecting, if you're using default identities, each connection will look like a new one.

We cheat in the above example by keeping state only for a very short time (the time it takes a worker to process a request) and then throwing away the state. But that's not practical for many cases. To properly manage client state in a stateful asynchronous server, you have to:

* Do heartbeating from client to server. In our example, we send a request once per second, which can reliably be used as a heartbeat.

* Store state using the client identity (whether generated or explicit) as key.

* Detect a stopped heartbeat. If there's no request from a client within, say, two seconds, the server can detect this and destroy any state it's holding for that client.

## Worked Example: Inter-Broker Routing {#Worked-Example-Inter-Broker-Routing}

Let's take everything we've seen so far, and scale things up to a real application. We'll build this step-by-step over several iterations. Our best client calls us urgently and asks for a design of a large cloud computing facility. He has this vision of a cloud that spans many data centers, each a cluster of clients and workers, and that works together as a whole. Because we're smart enough to know that practice always beats theory, we propose to make a working simulation using ZeroMQ. Our client, eager to lock down the budget before his own boss changes his mind, and having read great things about ZeroMQ on Twitter, agrees.

### Establishing the Details {#Establishing-the-Details}

Several espressos later, we want to jump into writing code, but a little voice tells us to get more details before making a sensational solution to entirely the wrong problem. "What kind of work is the cloud doing?", we ask.

The client explains:

* Workers run on various kinds of hardware, but they are all able to handle any task. There are several hundred workers per cluster, and as many as a dozen clusters in total.

* Clients create tasks for workers. Each task is an independent unit of work and all the client wants is to find an available worker, and send it the task, as soon as possible. There will be a lot of clients and they'll come and go arbitrarily.

* The real difficulty is to be able to add and remove clusters at any time. A cluster can leave or join the cloud instantly, bringing all its workers and clients with it.

* If there are no workers in their own cluster, clients' tasks will go off to other available workers in the cloud.

* Clients send out one task at a time, waiting for a reply. If they don't get an answer within X seconds, they'll just send out the task again. This isn't our concern; the client API does it already.

* Workers process one task at a time; they are very simple beasts. If they crash, they get restarted by whatever script started them.

So we double-check to make sure that we understood this correctly:

* "There will be some kind of super-duper network interconnect between clusters, right?", we ask. The client says, "Yes, of course, we're not idiots."

* "What kind of volumes are we talking about?", we ask. The client replies, "Up to a thousand clients per cluster, each doing at most ten requests per second. Requests are small, and replies are also small, no more than 1K bytes each."

So we do a little calculation and see that this will work nicely over plain TCP. 2,500 clients x 10/second x 1,000 bytes x 2 directions = 50MB/sec or 400Mb/sec, not a problem for a 1Gb network.

It's a straightforward problem that requires no exotic hardware or protocols, just some clever routing algorithms and careful design. We start by designing one cluster (one data center) and then we figure out how to connect clusters together.

### Architecture of a Single Cluster {#Architecture-of-a-Single-Cluster}

Workers and clients are synchronous. We want to use the load balancing pattern to route tasks to workers. Workers are all identical; our facility has no notion of different services. Workers are anonymous; clients never address them directly. We make no attempt here to provide guaranteed delivery, retry, and so on.

For reasons we already examined, clients and workers won't speak to each other directly. It makes it impossible to add or remove nodes dynamically. So our basic model consists of the request-reply message broker we saw earlier.

{{< textdiagram name="fig39.png" figno="39" title="Cluster Architecture" >}}
#--------#  #--------#  #--------#
| Client |  | Client |  | Client |
+--------+  +--------+  +--------+
|  REQ   |  |  REQ   |  |  REQ   |
'---+----'  '---+----'  '---+----'
    |           |           |
    '-----------+-----------'
                |
          .-----+------.
          |   ROUTER   |
          +------------+
          |    Load    |
          |  balancer  |  Broker
          +------------+
          |   ROUTER   |
          '-----+------'
                |
    .-----------+-----------.
    |           |           |
.---+----.  .---+----.  .---+----.
|  REQ   |  |  REQ   |  |  REQ   |
+--------+  +--------+  +--------+
| Worker |  | Worker |  | Worker |
#--------#  #--------#  #--------#
{{< /textdiagram >}}

### Scaling to Multiple Clusters {#Scaling-to-Multiple-Clusters}

Now we scale this out to more than one cluster. Each cluster has a set of clients and workers, and a broker that joins these together.

{{< textdiagram name="fig40.png" figno="40" title="Multiple Clusters" >}}
     Cluster 1          :          Cluster 2
                        :
.---.  .---.  .---.     :     .---.  .---.  .---.
| C |  | C |  | C |     :     | C |  | C |  | C |
'-+-'  '-+-'  '-+-'     :     '-+-'  '-+-'  #-+-'
  |      |      |       :       |      |      |
  |      |      |       :       |      |      |
#-+------+------+-#     :     #-+------+------+-#
|     Broker      |     :     |     Broker      |
#-+------+------+-#     :     #-+------+------+-#
  |      |      |       :       |      |      |
  |      |      |       :       |      |      |
.-+-.  .-+-.  .-+-.     :     .-+-.  .-+-.  .-+-.
| W |  | W |  | W |     :     | W |  | W |  | W |
'---'  '---'  '---'     :     '---'  '---'  '---'
{{< /textdiagram >}}

The question is: how do we get the clients of each cluster talking to the workers of the other cluster? There are a few possibilities, each with pros and cons:

* Clients could connect directly to both brokers. The advantage is that we don't need to modify brokers or workers. But clients get more complex and become aware of the overall topology. If we want to add a third or forth cluster, for example, all the clients are affected. In effect we have to move routing and failover logic into the clients and that's not nice.

* Workers might connect directly to both brokers. But REQ workers can't do that, they can only reply to one broker. We might use REPs but REPs don't give us customizable broker-to-worker routing like load balancing does, only the built-in load balancing. That's a fail; if we want to distribute work to idle workers, we precisely need load balancing. One solution would be to use ROUTER sockets for the worker nodes. Let's label this "Idea #1".

* Brokers could connect to each other. This looks neatest because it creates the fewest additional connections. We can't add clusters on the fly, but that is probably out of scope. Now clients and workers remain ignorant of the real network topology, and brokers tell each other when they have spare capacity. Let's label this "Idea #2".

Let's explore Idea #1. In this model, we have workers connecting to both brokers and accepting jobs from either one.

{{< textdiagram name="fig41.png" figno="41" title="Idea 1: Cross-connected Workers" >}}
            Cluster 1               :  Cluster 2
                                    :
            #--------#              :  #--------#
            |        |              :  |        |
            +--------+              :  +--------+
            | ROUTER |              :  | ROUTER |
            '---+----'              :  '---+----'
                |                   :      |
      .---------+-+-----------+-----+------'
      |         | |           |     :
    .-+---------+-+---------. |     :
    | |         | |         | |     :
    | |         | |         | |     :
.---+-+--.  .---+-+--.  .---+-+--.  :
| ROUTER |  | ROUTER |  | ROUTER |  :
+--------+  +--------+  +--------+  :
| Worker |  | Worker |  | Worker |  :
#--------#  #--------#  #--------#  :
{{< /textdiagram >}}

It looks feasible. However, it doesn't provide what we wanted, which was that clients get local workers if possible and remote workers only if it's better than waiting. Also workers will signal "ready" to both brokers and can get two jobs at once, while other workers remain idle. It seems this design fails because again we're putting routing logic at the edges.

So, idea #2 then. We interconnect the brokers and don't touch the clients or workers, which are REQs like we're used to.

{{< textdiagram name="fig42.png" figno="42" title="Idea 2: Brokers Talking to Each Other" >}}
     Cluster 1         :         Cluster 2
                       :
.---.  .---.  .---.    :    .---.  .---.  .---.
| C |  | C |  | C |    :    | C |  | C |  | C |
'-+-'  '-+-'  '-+-'    :    '-+-'  '-+-'  '-+-'
  |      |      |      :      |      |      |
  |      |      |      :      |      |      |
#-+------+------+-#    :    #-+------+------+-#
|     Broker      |<---+--->|     Broker      |
#-+------+------+-#    :    #-+------+------+-#
  |      |      |      :      |      |      |
  |      |      |      :      |      |      |
.-+-.  .-+-.  .-+-.    :    .-+-.  .-+-.  .-+-.
| W |  | W |  | W |    :    | W |  | W |  | W |
'---'  '---'  '---'    :    '---'  '---'  '---'
{{< /textdiagram >}}

This design is appealing because the problem is solved in one place, invisible to the rest of the world. Basically, brokers open secret channels to each other and whisper, like camel traders, "Hey, I've got some spare capacity. If you have too many clients, give me a shout and we'll deal".

In effect it is just a more sophisticated routing algorithm: brokers become subcontractors for each other. There are other things to like about this design, even before we play with real code:

* It treats the common case (clients and workers on the same cluster) as default and does extra work for the exceptional case (shuffling jobs between clusters).

* It lets us use different message flows for the different types of work. That means we can handle them differently, e.g., using different types of network connection.

* It feels like it would scale smoothly. Interconnecting three or more brokers doesn't get overly complex. If we find this to be a problem, it's easy to solve by adding a super-broker.

We'll now make a worked example. We'll pack an entire cluster into one process. That is obviously not realistic, but it makes it simple to simulate, and the simulation can accurately scale to real processes. This is the beauty of ZeroMQ--you can design at the micro-level and scale that up to the macro-level. Threads become processes, and then become boxes and the patterns and logic remain the same. Each of our "cluster" processes contains client threads, worker threads, and a broker thread.

We know the basic model well by now:

* The REQ client (REQ) threads create workloads and pass them to the broker (ROUTER).
* The REQ worker (REQ) threads process workloads and return the results to the broker (ROUTER).
* The broker queues and distributes workloads using the load balancing pattern.

### Federation Versus Peering {#Federation-Versus-Peering}

There are several possible ways to interconnect brokers. What we want is to be able to tell other brokers, "we have capacity", and then receive multiple tasks. We also need to be able to tell other brokers, "stop, we're full". It doesn't need to be perfect; sometimes we may accept jobs we can't process immediately, then we'll do them as soon as possible.

The simplest interconnect is *federation*, in which brokers simulate clients and workers for each other. We would do this by connecting our frontend to the other broker's backend socket. Note that it is legal to both bind a socket to an endpoint and connect it to other endpoints.

{{< textdiagram name="fig43.png" figno="43" title="Cross-connected Brokers in Federation Model" >}}
     Cluster 1          :         Cluster 2
                        :
.---.  .---.            :            .---.  .---.
| C |  | C |            :            | C |  | C |
'-+-'  '-+-'    .----.  :  .====.    '-+-'  '-+-'
  |      |      |    |  :  :    |      |      |
  |      |      |    |  :  :    |      |      |
#-+------+------+-#  |  :  :  #-+------+------+-#
|     Broker      |  |  :  :  |     Broker      |
#-+------+--------#  |  :  :  #--------+------+-#
  |      |      ^    |  :  :    ^      |      |
  |      |      :    |  :  :    |      |      |
.-+-.  .-+-.    :    '--+--+----'    .-+-.  .-+-.
| W |  | W |    :       :  :         | W |  | W |
'---'  '---'    :       :  :         '---'  '---'
                '=======+=='
                        :
                        :
{{< /textdiagram >}}

This would give us simple logic in both brokers and a reasonably good mechanism: when there are no workers, tell the other broker "ready", and accept one job from it. The problem is also that it is too simple for this problem. A federated broker would be able to handle only one task at a time. If the broker emulates a lock-step client and worker, it is by definition also going to be lock-step, and if it has lots of available workers they won't be used. Our brokers need to be connected in a fully asynchronous fashion.

The federation model is perfect for other kinds of routing, especially service-oriented architectures (SOAs), which route by service name and proximity rather than load balancing or round robin. So don't dismiss it as useless, it's just not right for all use cases.

Instead of federation, let's look at a *peering* approach in which brokers are explicitly aware of each other and talk over privileged channels. Let's break this down, assuming we want to interconnect N brokers. Each broker has (N - 1) peers, and all brokers are using exactly the same code and logic. There are two distinct flows of information between brokers:

* Each broker needs to tell its peers how many workers it has available at any time. This can be fairly simple information--just a quantity that is updated regularly. The obvious (and correct) socket pattern for this is pub-sub. So every broker opens a PUB socket and publishes state information on that, and every broker also opens a SUB socket and connects that to the PUB socket of every other broker to get state information from its peers.

* Each broker needs a way to delegate tasks to a peer and get replies back, asynchronously. We'll do this using ROUTER sockets; no other combination works. Each broker has two such sockets: one for tasks it receives and one for tasks it delegates. If we didn't use two sockets, it would be more work to know whether we were reading a request or a reply each time. That would mean adding more information to the message envelope.

And there is also the flow of information between a broker and its local clients and workers.

### The Naming Ceremony {#The-Naming-Ceremony}

Three flows x two sockets for each flow = six sockets that we have to manage in the broker.  Choosing good names is vital to keeping a multisocket juggling act reasonably coherent in our minds. Sockets *do* something and what they do should form the basis for their names. It's about being able to read the code several weeks later on a cold Monday morning before coffee, and not feel any pain.

Let's do a shamanistic naming ceremony for the sockets. The three flows are:

* A *local* request-reply flow between the broker and its clients and workers.
* A *cloud* request-reply flow between the broker and its peer brokers.
* A *state* flow between the broker and its peer brokers.

Finding meaningful names that are all the same length means our code will align nicely. It's not a big thing, but attention to details helps. For each flow the broker has two sockets that we can orthogonally call the *frontend* and *backend*. We've used these names quite often. A frontend receives information or tasks. A backend sends those out to other peers. The conceptual flow is from front to back (with replies going in the opposite direction from back to front).

So in all the code we write for this tutorial, we will use these socket names:

* *localfe* and *localbe* for the local flow.
* *cloudfe* and *cloudbe* for the cloud flow.
* *statefe* and *statebe* for the state flow.

For our transport and because we're simulating the whole thing on one box, we'll use <tt>ipc</tt> for everything. This has the advantage of working like <tt>tcp</tt> in terms of connectivity (i.e., it's a disconnected transport, unlike <tt>inproc</tt>), yet we don't need IP addresses or DNS names, which would be a pain here. Instead, we will use <tt>ipc</tt> endpoints called *something*-<tt>local</tt>, *something*-<tt>cloud</tt>, and *something*-<tt>state</tt>, where *something* is the name of our simulated cluster.

You might be thinking that this is a lot of work for some names. Why not call them s1, s2, s3, s4, etc.? The answer is that if your brain is not a perfect machine, you need a lot of help when reading code, and we'll see that these names do help. It's easier to remember "three flows, two directions" than "six different sockets".

{{< textdiagram name="fig44.png" figno="44" title="Broker Socket Arrangement" >}}
#---------#  #---------#  #---------#
| Client  |  | Broker  |  | Broker  |
|         |  | cloudbe |  | statebe |
+---------+  +---------+  +---------+
| connect |  | connect |  |  bind   |
'---------'  '---------'  '---------'
  request      request       state
     \            |
      \           |           /
       v          v          v
   .---------+---------+---------.
   |  bind   |  bind   | connect |
   +---------+---------+---------+
   | localfe | cloudfe | statefe |   Frontends
   | ROUTER  | ROUTER  |   SUB   |   (incoming)
   +---------+---------+---------+
   |           Broker            |
   +---------+---------+---------+
   | ROUTER  | ROUTER  |   PUB   |   Backends
   | localbe | cloudbe | statebe |   (outgoing)
   +---------+---------+---------+
   |  bind   | connect |  bind   |
   '---------+---------+---------'
     request    request   state
                  |          \
      /           |           \
     v            v            v
.---------.  .---------.  .---------.
| connect |  |  bind   |  | connect |
+---------+  +---------+  +---------+
| Worker  |  | Broker  |  | Broker  |
|         |  | cloudfe |  | statefe |
#---------#  #---------#  #---------#
{{< /textdiagram >}}

Note that we connect the cloudbe in each broker to the cloudfe in every other broker, and likewise we connect the statebe in each broker to the statefe in every other broker.

### Prototyping the State Flow {#Prototyping-the-State-Flow}

Because each socket flow has its own little traps for the unwary, we will test them in real code one-by-one, rather than try to throw the whole lot into code in one go. When we're happy with each flow, we can put them together into a full program. We'll start with the state flow.

{{< textdiagram name="fig45.png" figno="45" title="The State Flow" >}}
           #---------#
           | Broker  |
           | statebe |
           +---------+
           |  bind   |
           '---------'
              state

               /
              v
.--------+---------.
|        | connect |
|        +---------+
|        | statefe |
|        |   SUB   |
+--------+---------+
|     Broker       |
+--------+---------+
|        |   PUB   |
|        | statebe |
|        +---------+
|        |  bind   |
'--------+---------'
            state
              \
               \
                v
           .---------.
           | connect |
           +---------+
           | statefe |
           | Broker  |
           #---------#
{{< /textdiagram >}}

Here is how this works in code:

{{< examples name="peering1" title="Prototype state flow" >}}

Notes about this code:

* Each broker has an identity that we use to construct <tt>ipc</tt> endpoint names. A real broker would need to work with TCP and a more sophisticated configuration scheme. We'll look at such schemes later in this book, but for now, using generated <tt>ipc</tt> names lets us ignore the problem of where to get TCP/IP addresses or names.

* We use a <tt>[zmq_poll()](http://api.zeromq.org/master:zmq_poll)</tt> loop as the core of the program. This processes incoming messages and sends out state messages. We send a state message *only* if we did not get any incoming messages *and* we waited for a second. If we send out a state message each time we get one in, we'll get message storms.

* We use a two-part pub-sub message consisting of sender address and data. Note that we will need to know the address of the publisher in order to send it tasks, and the only way is to send this explicitly as a part of the message.

* We don't set identities on subscribers because if we did then we'd get outdated state information when connecting to running brokers.

* We don't set a HWM on the publisher, but if we were using ZeroMQ v2.x that would be a wise idea.

We can build this little program and run it three times to simulate three clusters. Let's call them DC1, DC2, and DC3 (the names are arbitrary). We run these three commands, each in a separate window:

```
peering1 DC1 DC2 DC3  #  Start DC1 and connect to DC2 and DC3
peering1 DC2 DC1 DC3  #  Start DC2 and connect to DC1 and DC3
peering1 DC3 DC1 DC2  #  Start DC3 and connect to DC1 and DC2
```

You'll see each cluster report the state of its peers, and after a few seconds they will all happily be printing random numbers once per second. Try this and satisfy yourself that the three brokers all match up and synchronize to per-second state updates.

In real life, we'd not send out state messages at regular intervals, but rather whenever we had a state change, i.e., whenever a worker becomes available or unavailable. That may seem like a lot of traffic, but state messages are small and we've established that the inter-cluster connections are super fast.

If we wanted to send state messages at precise intervals, we'd create a child thread and open the <tt>statebe</tt> socket in that thread. We'd then send irregular state updates to that child thread from our main thread and allow the child thread to conflate them into regular outgoing messages. This is more work than we need here.

### Prototyping the Local and Cloud Flows {#Prototyping-the-Local-and-Cloud-Flows}

Let's now prototype the flow of tasks via the local and cloud sockets. This code pulls requests from clients and then distributes them to local workers and cloud peers on a random basis.

{{< textdiagram name="fig46.png" figno="46" title="The Flow of Tasks" >}}
#---------#  #---------#
| Client  |  | Broker  |
|         |  | cloudbe |
+---------+  +---------+
| connect |  | connect |
'---------'  '---------'
  request      request
     \            |
      \           |
       v          v
   .---------+---------+---.
   |  bind   |  bind   |   |
   +---------+---------+   |
   | localfe | cloudfe |   |
   | ROUTER  | ROUTER  |   |
   +---------+---------+---+
   |         Broker        |
   +---------+---------+---+
   | ROUTER  | ROUTER  |   |
   | localbe | cloudbe |   |
   +---------+---------+   |
   |  bind   | connect |   |
   '---------+---------+---'
     request    request
                  |
      /           |
     v            v
.---------.  .---------.
| connect |  |  bind   |
+---------+  +---------+
|         |  | cloudfe |
| Worker  |  | Broker  |
#---------#  #---------#
{{< /textdiagram >}}

Before we jump into the code, which is getting a little complex, let's sketch the core routing logic and break it down into a simple yet robust design.

We need two queues, one for requests from local clients and one for requests from cloud clients. One option would be to pull messages off the local and cloud frontends, and pump these onto their respective queues. But this is kind of pointless because ZeroMQ sockets *are* queues already. So let's use the ZeroMQ socket buffers as queues.

This was the technique we used in the load balancing broker, and it worked nicely. We only read from the two frontends when there is somewhere to send the requests. We can always read from the backends, as they give us replies to route back. As long as the backends aren't talking to us, there's no point in even looking at the frontends.

So our main loop becomes:

* Poll the backends for activity. When we get a message, it may be "ready" from a worker or it may be a reply. If it's a reply, route back via the local or cloud frontend.

* If a worker replied, it became available, so we queue it and count it.

* While there are workers available, take a request, if any, from either frontend and route to a local worker, or randomly, to a cloud peer.

Randomly sending tasks to a peer broker rather than a worker simulates work distribution across the cluster. It's dumb, but that is fine for this stage.

We use broker identities to route messages between brokers. Each broker has a name that we provide on the command line in this simple prototype. As long as these names don't overlap with the ZeroMQ-generated UUIDs used for client nodes, we can figure out whether to route a reply back to a client or to a broker.

Here is how this works in code. The interesting part starts around the comment "Interesting part".

{{< examples name="peering2" title="Prototype local and cloud flow" >}}

Run this by, for instance, starting two instances of the broker in two windows:

```
peering2 me you
peering2 you me
```

Some comments on this code:

* In the C code at least, using the zmsg class makes life much easier, and our code much shorter. It's obviously an abstraction that works. If you build ZeroMQ applications in C, you should use CZMQ.

* Because we're not getting any state information from peers, we naively assume they are running. The code prompts you to confirm when you've started all the brokers. In the real case, we'd not send anything to brokers who had not told us they exist.

You can satisfy yourself that the code works by watching it run forever. If there were any misrouted messages, clients would end up blocking, and the brokers would stop printing trace information. You can prove that by killing either of the brokers. The other broker tries to send requests to the cloud, and one-by-one its clients block, waiting for an answer.

### Putting it All Together {#Putting-it-All-Together}

Let's put this together into a single package. As before, we'll run an entire cluster as one process. We're going to take the two previous examples and merge them into one properly working design that lets you simulate any number of clusters.

This code is the size of both previous prototypes together, at 270 LoC. That's pretty good for a simulation of a cluster that includes clients and workers and cloud workload distribution. Here is the code:

{{< examples name="peering3" title="Full cluster simulation" >}}

It's a nontrivial program and took about a day to get working. These are the highlights:

* The client threads detect and report a failed request. They do this by polling for a response and if none arrives after a while (10 seconds), printing an error message.

* Client threads don't print directly, but instead send a message to a monitor socket (PUSH) that the main loop collects (PULL) and prints off. This is the first case we've seen of using ZeroMQ sockets for monitoring and logging; this is a big use case that we'll come back to later.

* Clients simulate varying loads to get the cluster 100% at random moments, so that tasks are shifted over to the cloud. The number of clients and workers, and delays in the client and worker threads control this. Feel free to play with them to see if you can make a more realistic simulation.

* The main loop uses two pollsets. It could in fact use three: information, backends, and frontends. As in the earlier prototype, there is no point in taking a frontend message if there is no backend capacity.

These are some of the problems that arose during development of this program:

* Clients would freeze, due to requests or replies getting lost somewhere. Recall that the ROUTER socket drops messages it can't route. The first tactic here was to modify the client thread to detect and report such problems. Secondly, I put <tt>zmsg_dump()</tt> calls after every receive and before every send in the main loop, until the origin of the problems was clear.

* The main loop was mistakenly reading from more than one ready socket. This caused the first message to be lost. I fixed that by reading only from the first ready socket.

* The <tt>zmsg</tt> class was not properly encoding UUIDs as C strings. This caused UUIDs that contain 0 bytes to be corrupted. I fixed that by modifying <tt>zmsg</tt> to encode UUIDs as printable hex strings.

This simulation does not detect disappearance of a cloud peer. If you start several peers and stop one, and it was broadcasting capacity to the others, they will continue to send it work even if it's gone. You can try this, and you will get clients that complain of lost requests. The solution is twofold: first, only keep the capacity information for a short time so that if a peer does disappear, its capacity is quickly set to zero. Second, add reliability to the request-reply chain. We'll look at reliability in the next chapter.

