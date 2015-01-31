ZeroMQ Examples in C#
---

Hello! I've made some new examples for C#.

You can open the `ZGuideExamples.*.csproj` in Visual C# on Windows or in MonoDevelop on Linux.  
Add a Reference to the project [`/zeromq/clrzmq4`](http://github.com/zeromq/clrzmq4) (or the release ZeroMQ.dll).

Now compile the project and run `./ZGuideExamples.exe` !

```
Usage: ./ZGuideExamples.exe [--option=++] [--option=tcp://192.168.1.1:8080] <command> World Me You
```

Available Examples:

- HWServer, HWClient

```
	Usage: ./ZGuideExamples.exe HWClient
```

```
	Usage: ./ZGuideExamples.exe HWServer [Name]

        Name   Your name. Default: World
```

- version

```
	Usage: ./ZGuideExamples.exe Version
```

- wuserver, wuclient, wuproxy

```
	Usage: ./ZGuideExamples.exe WUServer
```

```
	Usage: ./ZGuideExamples.exe WUClient [ZipCode] [Endpoint]

        ZipCode   The zip code to subscribe. Default is 72622 Nürtingen
	    Endpoint  Where WUClient should connect to.
	              Default is tcp://127.0.0.1:5556
```

```
	Usage: ./ZGuideExamples.exe WUProxy
```

- taskvent, taskwork, tasksink

```
	Usage: ./ZGuideExamples.exe TaskVent
```

```
	Usage: ./ZGuideExamples.exe TaskWork
```

```
	Usage: ./ZGuideExamples.exe TaskSink
```

- msreader, mspoller

```
	Usage: ./ZGuideExamples.exe MSReader
```

```
	Usage: ./ZGuideExamples.exe MSPoller
```

- rrclient, rrworker, rrbroker

```
	Usage: ./ZGuideExamples.exe RRClient
```

```
	Usage: ./ZGuideExamples.exe RRWorker [Name] [Endpoint]

        Name      Your Name
	    Endpoint  Where RRClient should connect to.
	              Default is tcp://127.0.0.1:5559
```

```
	Usage: ./ZGuideExamples.exe RRBroker
```

- msgqueue

```
	Usage: ./ZGuideExamples.exe msgqueue
```

- taskwork2, tasksink2

Use with TaskVent.

```
	Usage: ./ZGuideExamples.exe TaskWork2
```

```
	Usage: ./ZGuideExamples.exe TaskSink2
```

- interrupt

Use with HWClient.

```
	Usage: ./ZGuideExamples.exe Interrupt
```

- mtserver, mtrelay

```
	Usage: ./ZGuideExamples.exe MTServer

	Usage: ./ZGuideExamples.exe MTRelay
```

- syncpub, syncsub

```
	Usage: ./ZGuideExamples.exe SyncPub

	Usage: ./ZGuideExamples.exe SyncSub
```

- psenvpub, psenvsub

```
	Usage: ./ZGuideExamples.exe PSEnvPub

	Usage: ./ZGuideExamples.exe PSEnvSub
```

- identity

```
	Usage: ./ZGuideExamples.exe Identity
```

- rtreq, rtdealer

```
	Usage: ./ZGuideExamples.exe RTReq

	Usage: ./ZGuideExamples.exe RTDealer
```

- lbbroker

```
	Usage: ./ZGuideExamples.exe LBBroker
```

- asyncsrv

```
	Usage: ./ZGuideExamples.exe AsyncSrv
```

- peering1, peering2

```
	Usage: ./ZGuideExamples.exe Peering1 World Receiver0
				                Peering1 Receiver0 World

	Usage: ./ZGuideExamples.exe Peering2
```

- lpclient, lpserver

```
	Usage: ./ZGuideExamples.exe LPClient [Name]

        Name   Your name. Default: World

	Usage: ./ZGuideExamples.exe LPServer
```

- spqueue, spworker

```
	Usage: ./ZGuideExamples.exe SPQueue

	Usage: ./ZGuideExamples.exe SPWorker
```

- ppqueue, ppworker

```
	Usage: ./ZGuideExamples.exe PPQueue

	Usage: ./ZGuideExamples.exe PPWorker [Name]

        Name   Your name. Default: World
```

- flserver1, flclient1

```
	Usage: ./ZGuideExamples.exe FLServer1 [Endpoint]

	    Endpoint  Where FLServer1 should bind on.
	              Default is tcp://127.0.0.1:7780

	Usage: ./ZGuideExamples.exe FLClient1 [Endpoint]

	    Endpoint  Where FLClient1 should connect to.
	              Default is tcp://127.0.0.1:7780
```

- flserver2, flclient2

```
	Usage: ./ZGuideExamples.exe FLServer2 [Endpoint]

	    Endpoint  Where FLServer2 should bind on.
	              Default is tcp://127.0.0.1:7780

	Usage: ./ZGuideExamples.exe FLClient2 [Endpoint] ...

	    Endpoint  Where FLClient2 should connect to.
	              Default is tcp://127.0.0.1:7780
```

- flserver3, flclient3, flcliapi

Have a look on the FLCliApi.FreelanceClient!

```
	Usage: ./ZGuideExamples.exe FLServer3

	Usage: ./ZGuideExamples.exe FLClient3 [Name] [Endpoint]

	    Name      Your Name
	    Endpoint  Where FLClient3 should connect to.
	              Default: tcp://127.0.0.1:5555
```

- espresso

```
	Usage: ./ZGuideExamples.exe Espresso
```

- pathopub, pathosub, lvcache

```
	Usage: ./ZGuideExamples.exe PathoPub [Endpoint]

	    Endpoint  Where PathoPub should connect to.
	              Default is null, Binding on tcp://*:5556

	Usage: ./ZGuideExamples.exe PathoSub [Endpoint]

	    Endpoint  Where PathoSub should connect to.
	              Default is tcp://127.0.0.1:5556

	Usage: ./ZGuideExamples.exe LVCache

        The LVCache is binding on tcp://*:5557 and tcp://*.5558
```

- suisnail

```
	Usage: ./ZGuideExamples.exe SuiSnail
```

- eagain

```
	Usage: ./ZGuideExamples.exe EAgain
```

