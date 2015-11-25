ZeroMQ Examples in C#
---

Hello! I've made some new examples for C#.

Also read: [ZeroMQ - The Guide](http://zguide.zeromq.org/page:all). Current version: [ZeroMQ - The Guide Examples](https://github.com/metadings/zguide/tree/master/examples/C%23/).

You can open the `ZGuideExamples.*.csproj` in Visual C# on Windows or in MonoDevelop on Linux.  
Add a Reference to the project [`/zeromq/clrzmq4`](http://github.com/zeromq/clrzmq4) (or the release ZeroMQ.dll).

Now compile the project and run `./ZGuideExamples.exe` !

```
Usage: ./ZGuideExamples.exe [--option=++] [--option=tcp://192.168.1.1:8080] <command> World Me You
```

#### [HWServer](https://github.com/metadings/zguide/blob/master/examples/C%23/hwserver.cs), [HWClient](https://github.com/metadings/zguide/blob/master/examples/C%23/hwclient.cs)

```
	Usage: ./ZGuideExamples.exe HWClient

	Usage: ./ZGuideExamples.exe HWServer [Name]

        Name   Your name. Default: World
```

#### [Version](https://github.com/metadings/zguide/blob/master/examples/C%23/version.cs)

```
	Usage: ./ZGuideExamples.exe Version
```

#### [WUServer](https://github.com/metadings/zguide/blob/master/examples/C%23/wuserver.cs), [WUClient](https://github.com/metadings/zguide/blob/master/examples/C%23/wuclient.cs), [WUProxy](https://github.com/metadings/zguide/blob/master/examples/C%23/wuproxy.cs)

```
	Usage: ./ZGuideExamples.exe WUServer

	Usage: ./ZGuideExamples.exe WUClient [ZipCode] [Endpoint]

        ZipCode   The zip code to subscribe. Default is 72622 Nürtingen
	    Endpoint  Where WUClient should connect to.
	              Default is tcp://127.0.0.1:5556

	Usage: ./ZGuideExamples.exe WUProxy
```

#### [TaskVent](https://github.com/metadings/zguide/blob/master/examples/C%23/taskvent.cs), [TaskWork](https://github.com/metadings/zguide/blob/master/examples/C%23/taskwork.cs), [TaskSink](https://github.com/metadings/zguide/blob/master/examples/C%23/tasksink.cs)

```
	Usage: ./ZGuideExamples.exe TaskVent

	Usage: ./ZGuideExamples.exe TaskWork

	Usage: ./ZGuideExamples.exe TaskSink
```

#### [MSReader](https://github.com/metadings/zguide/blob/master/examples/C%23/msreader.cs), [MSPoller](https://github.com/metadings/zguide/blob/master/examples/C%23/mspoller.cs)

```
	Usage: ./ZGuideExamples.exe MSReader

	Usage: ./ZGuideExamples.exe MSPoller
```

#### [RRClient](https://github.com/metadings/zguide/blob/master/examples/C%23/rrclient.cs), [RRWorker](https://github.com/metadings/zguide/blob/master/examples/C%23/rrworker.cs), [RRBroker](https://github.com/metadings/zguide/blob/master/examples/C%23/rrbroker.cs)

```
	Usage: ./ZGuideExamples.exe RRClient

	Usage: ./ZGuideExamples.exe RRWorker [Name] [Endpoint]

        Name      Your Name
	    Endpoint  Where RRClient should connect to.
	              Default is tcp://127.0.0.1:5559

	Usage: ./ZGuideExamples.exe RRBroker
```

#### [MsgQueue](https://github.com/metadings/zguide/blob/master/examples/C%23/msgqueue.cs)

Use with RRServer and RRClient.

```
	Usage: ./ZGuideExamples.exe MsgQueue
```

#### [TaskWork2](https://github.com/metadings/zguide/blob/master/examples/C%23/taskwork2.cs), [TaskSink2](https://github.com/metadings/zguide/blob/master/examples/C%23/tasksink2.cs)

Use with TaskVent.

```
	Usage: ./ZGuideExamples.exe TaskWork2

	Usage: ./ZGuideExamples.exe TaskSink2
```

#### [Interrupt](https://github.com/metadings/zguide/blob/master/examples/C%23/interrupt.cs)

Use with HWClient.

```
	Usage: ./ZGuideExamples.exe Interrupt [Name]

        Name      Your Name
```

#### [MTServer](https://github.com/metadings/zguide/blob/master/examples/C%23/mtserver.cs), [MTRelay](https://github.com/metadings/zguide/blob/master/examples/C%23/mtrelay.cs)

```
	Usage: ./ZGuideExamples.exe MTServer

	Usage: ./ZGuideExamples.exe MTRelay
```

#### [SyncPub](https://github.com/metadings/zguide/blob/master/examples/C%23/syncpub.cs), [SyncSub](https://github.com/metadings/zguide/blob/master/examples/C%23/syncsub.cs)

```
	Usage: ./ZGuideExamples.exe SyncPub

	Usage: ./ZGuideExamples.exe SyncSub
```

#### [PSEnvPub](https://github.com/metadings/zguide/blob/master/examples/C%23/psenvpub.cs), [PSEnvSub](https://github.com/metadings/zguide/blob/master/examples/C%23/psenvsub.cs)

```
	Usage: ./ZGuideExamples.exe PSEnvPub

	Usage: ./ZGuideExamples.exe PSEnvSub
```

#### [Identity](https://github.com/metadings/zguide/blob/master/examples/C%23/identity.cs)

```
	Usage: ./ZGuideExamples.exe Identity
```

#### [RTReq](https://github.com/metadings/zguide/blob/master/examples/C%23/rtreq.cs), [RTDealer](https://github.com/metadings/zguide/blob/master/examples/C%23/rtdealer.cs)

```
	Usage: ./ZGuideExamples.exe RTReq

	Usage: ./ZGuideExamples.exe RTDealer
```

#### [LBBroker](https://github.com/metadings/zguide/blob/master/examples/C%23/lbbroker.cs)

```
	Usage: ./ZGuideExamples.exe LBBroker
```

#### [AsyncSrv](https://github.com/metadings/zguide/blob/master/examples/C%23/asyncsrv.cs)

```
	Usage: ./ZGuideExamples.exe AsyncSrv
```

#### [Peering1](https://github.com/metadings/zguide/blob/master/examples/C%23/peering1.cs), [Peering2](https://github.com/metadings/zguide/blob/master/examples/C%23/peering2.cs)

```
	Usage: ./ZGuideExamples.exe Peering1 World Receiver0
				                Peering1 Receiver0 World

	Usage: ./ZGuideExamples.exe Peering2 World Receiver0
				                Peering2 Receiver0 World
```

#### [LPClient](https://github.com/metadings/zguide/blob/master/examples/C%23/lpclient.cs), [LPServer](https://github.com/metadings/zguide/blob/master/examples/C%23/lpserver.cs)

```
	Usage: ./ZGuideExamples.exe LPClient [Name]

        Name   Your name. Default: World

	Usage: ./ZGuideExamples.exe LPServer
```

#### [SPQueue](https://github.com/metadings/zguide/blob/master/examples/C%23/spqueue.cs), [SPWorker](https://github.com/metadings/zguide/blob/master/examples/C%23/spworker.cs)

Use with LPClient.

```
	Usage: ./ZGuideExamples.exe SPQueue

	Usage: ./ZGuideExamples.exe SPWorker
```

#### [PPQueue](https://github.com/metadings/zguide/blob/master/examples/C%23/ppqueue.cs), [PPWorker](https://github.com/metadings/zguide/blob/master/examples/C%23/ppworker.cs)

Use with LPClient.

```
	Usage: ./ZGuideExamples.exe PPQueue

	Usage: ./ZGuideExamples.exe PPWorker [Name]

        Name   Your name. Default: World
```

#### [FLServer1](https://github.com/metadings/zguide/blob/master/examples/C%23/flserver1.cs), [FLClient1](https://github.com/metadings/zguide/blob/master/examples/C%23/flclient1.cs)

```
	Usage: ./ZGuideExamples.exe FLServer1 [Endpoint]

	    Endpoint  Where FLServer1 should bind on.
	              Default is tcp://127.0.0.1:7780

	Usage: ./ZGuideExamples.exe FLClient1 [Endpoint]

	    Endpoint  Where FLClient1 should connect to.
	              Default is tcp://127.0.0.1:7780
```

#### [FLServer2](https://github.com/metadings/zguide/blob/master/examples/C%23/flserver2.cs), [FLClient2](https://github.com/metadings/zguide/blob/master/examples/C%23/flclient2.cs)

```
	Usage: ./ZGuideExamples.exe FLServer2 [Endpoint]

	    Endpoint  Where FLServer2 should bind on.
	              Default is tcp://127.0.0.1:7781

	Usage: ./ZGuideExamples.exe FLClient2 [Endpoint] ...

	    Endpoint  Where FLClient2 should connect to.
	              Default is tcp://127.0.0.1:7781
```

#### [FLServer3](https://github.com/metadings/zguide/blob/master/examples/C%23/flserver3.cs), [FLClient3](https://github.com/metadings/zguide/blob/master/examples/C%23/flclient3.cs), [FLCliApi.FreelanceClient](https://github.com/metadings/zguide/blob/master/examples/C%23/flcliapi.cs)

```
	Usage: ./ZGuideExamples.exe [--verbose] FLServer3

	Usage: ./ZGuideExamples.exe FLClient3 [Name] [Endpoint]

	    Name      Your Name
	    Endpoint  Where FLClient3 should connect to.
	              Default: tcp://127.0.0.1:5555
```

#### [Espresso](https://github.com/metadings/zguide/blob/master/examples/C%23/espresso.cs)

```
	Usage: ./ZGuideExamples.exe Espresso
```

#### [PathoPub](https://github.com/metadings/zguide/blob/master/examples/C%23/pathopub.cs), [PathoSub](https://github.com/metadings/zguide/blob/master/examples/C%23/pathosub.cs), [LVCache](https://github.com/metadings/zguide/blob/master/examples/C%23/lvcache.cs)

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

#### [SuiSnail](https://github.com/metadings/zguide/blob/master/examples/C%23/suisnail.cs)

```
	Usage: ./ZGuideExamples.exe SuiSnail
```

#### [EAgain](https://github.com/metadings/zguide/blob/master/examples/C%23/eagain.cs)

```
	Usage: ./ZGuideExamples.exe EAgain
```

#### [MDBroker](https://github.com/metadings/zguide/blob/master/examples/C%23/mdbroker.cs), [MDWorker](https://github.com/metadings/zguide/blob/master/examples/C%23/mdworker.cs), [MDClient](https://github.com/metadings/zguide/blob/master/examples/C%23/mdclient.cs)

```
	Usage: ./ZGuideExamples.exe MDBroker [-v] [--verbose]

	    -v 		Verbose mode activated
	            Default verbose is deactivated
				
	Usage: ./ZGuideExamples.exe MDWorker [-v] [--verbose]

	    -v 		Verbose mode activated
	            Default verbose is deactivated
				
	Usage: ./ZGuideExamples.exe MDClient [-v] [--verbose]

	    -v 		Verbose mode activated
	            Default verbose is deactivated
```
