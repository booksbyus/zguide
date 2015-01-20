ZeroMQ Examples in C#
---

Hello! I've made some new examples for C#.

You can open the `ZGuideExamples.csproj` in VisualC# on Windows or in MonoDevelop on Linux.
Add a Reference to the project [`/zeromq/clrzmq4`](http://github.com/zeromq/clrzmq4) (or the release ZeroMQ.dll).

Now compile the project and run `./ZGuideExamples` !

```
Usage: ./ZGuideExamples.exe [--option=++] [--option=tcp://192.168.1.1:8080] <command> World Edward Ulrich

Available [option]s:


Available <command>s:

    AsyncSrv
    HWClient
    HWServer
    Identity
    Interrupt
    LBBroker
    LPClient
    LPServer
    MsgQueue
    MSPoller
    MSReader
    MTRelay
    MTServer
    Peering1
    Peering2
    PPQueue
    PSEnvPub
    PSEnvSub
    RRBroker
    RRClient
    RRServer
    RTDealer
    RTReq
    SPQueue
    SPWorker
    SyncPub
    SyncSub
    TaskSink
    TaskSink2
    TaskVent
    TaskWork
    TaskWork2
    ToHexBytes
    ToHexString
    Version
    WUClient
    WUProxy
    WUServer

```
