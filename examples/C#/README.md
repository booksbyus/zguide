# ZeroMQ Examples in C#

Hello! I've made some new examples for C#.

You can open the `ZGuideExamples.*.csproj` in Visual C# on Windows or in MonoDevelop on Linux.  
Add a Reference to the project [`/zeromq/clrzmq4`](http://github.com/zeromq/clrzmq4) (or the release ZeroMQ.dll).

Now compile the project and run `./ZGuideExamples.exe` !

```
Usage: ./ZGuideExamples.exe [--option=++] [--option=tcp://192.168.1.1:8080] <command> World Me You
```

### HWServer, HWClient

```
	Usage: ./ZGuideExamples.exe HWClient

	Usage: ./ZGuideExamples.exe HWServer [Name]

        Name   Your name. Default: World
```

### Version

```
	Usage: ./ZGuideExamples.exe Version
```

### WUServer, WUClient, WUProxy

```
	Usage: ./ZGuideExamples.exe WUServer

	Usage: ./ZGuideExamples.exe WUClient [ZipCode] [Endpoint]

        ZipCode   The zip code to subscribe. Default is 72622 Nürtingen
	    Endpoint  Where WUClient should connect to.
	              Default is tcp://127.0.0.1:5556

	Usage: ./ZGuideExamples.exe WUProxy
```

### TaskVent, TaskWork, TaskSink

```
	Usage: ./ZGuideExamples.exe TaskVent

	Usage: ./ZGuideExamples.exe TaskWork

	Usage: ./ZGuideExamples.exe TaskSink
```

### MSReader, MSPoller

```
	Usage: ./ZGuideExamples.exe MSReader

	Usage: ./ZGuideExamples.exe MSPoller
```

### RRClient, RRWorker, RRBroker

```
	Usage: ./ZGuideExamples.exe RRClient

	Usage: ./ZGuideExamples.exe RRWorker [Name] [Endpoint]

        Name      Your Name
	    Endpoint  Where RRClient should connect to.
	              Default is tcp://127.0.0.1:5559

	Usage: ./ZGuideExamples.exe RRBroker
```

### MsgQueue

```
	Usage: ./ZGuideExamples.exe msgqueue
```

### TaskWork2, TaskSink2

Use with TaskVent.

```
	Usage: ./ZGuideExamples.exe TaskWork2

	Usage: ./ZGuideExamples.exe TaskSink2
```

### Interrupt

Use with HWClient.

```
	Usage: ./ZGuideExamples.exe Interrupt
```

### MTServer, MTRelay

```
	Usage: ./ZGuideExamples.exe MTServer

	Usage: ./ZGuideExamples.exe MTRelay
```

### SyncPub, SyncSub

```
	Usage: ./ZGuideExamples.exe SyncPub

	Usage: ./ZGuideExamples.exe SyncSub
```

### PSEnvPub, PSEnvSub

```
	Usage: ./ZGuideExamples.exe PSEnvPub

	Usage: ./ZGuideExamples.exe PSEnvSub
```

### Identity

```
	Usage: ./ZGuideExamples.exe Identity
```

### RTReq, RTDealer

```
	Usage: ./ZGuideExamples.exe RTReq

	Usage: ./ZGuideExamples.exe RTDealer
```

### LBBroker

```
	Usage: ./ZGuideExamples.exe LBBroker
```

### AsyncSrv

```
	Usage: ./ZGuideExamples.exe AsyncSrv
```

### Peering1, Peering2

```
	Usage: ./ZGuideExamples.exe Peering1 World Receiver0
				                Peering1 Receiver0 World

	Usage: ./ZGuideExamples.exe Peering2
```

### LPClient, LPServer

```
	Usage: ./ZGuideExamples.exe LPClient [Name]

        Name   Your name. Default: World

	Usage: ./ZGuideExamples.exe LPServer
```

### SPQueue, SPWorker

```
	Usage: ./ZGuideExamples.exe SPQueue

	Usage: ./ZGuideExamples.exe SPWorker
```

### PPQueue, PPWorker

```
	Usage: ./ZGuideExamples.exe PPQueue

	Usage: ./ZGuideExamples.exe PPWorker [Name]

        Name   Your name. Default: World
```

### FLServer1, FLClient1

```
	Usage: ./ZGuideExamples.exe FLServer1 [Endpoint]

	    Endpoint  Where FLServer1 should bind on.
	              Default is tcp://127.0.0.1:7780

	Usage: ./ZGuideExamples.exe FLClient1 [Endpoint]

	    Endpoint  Where FLClient1 should connect to.
	              Default is tcp://127.0.0.1:7780
```

### FLServer2, FLClient2

```
	Usage: ./ZGuideExamples.exe FLServer2 [Endpoint]

	    Endpoint  Where FLServer2 should bind on.
	              Default is tcp://127.0.0.1:7780

	Usage: ./ZGuideExamples.exe FLClient2 [Endpoint] ...

	    Endpoint  Where FLClient2 should connect to.
	              Default is tcp://127.0.0.1:7780
```

### FLServer3, FLClient3, FLCliApi

Have a look on the [FLCliApi.FreelanceClient](https://github.com/metadings/zguide/blob/master/examples/C%23/flcliapi.cs)!

```
	Usage: ./ZGuideExamples.exe FLServer3

	Usage: ./ZGuideExamples.exe FLClient3 [Name] [Endpoint]

	    Name      Your Name
	    Endpoint  Where FLClient3 should connect to.
	              Default: tcp://127.0.0.1:5555
```

### Espresso

```
	Usage: ./ZGuideExamples.exe Espresso
```

### PathoPub, PathoSub, LVCache

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

### SuiSnail

```
	Usage: ./ZGuideExamples.exe SuiSnail
```

### EAgain

```
	Usage: ./ZGuideExamples.exe EAgain
```

