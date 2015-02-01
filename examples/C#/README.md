# ZeroMQ Examples in C#

Hello! I've made some new examples for C#.

You can open the `ZGuideExamples.*.csproj` in Visual C# on Windows or in MonoDevelop on Linux.  
Add a Reference to the project [`/zeromq/clrzmq4`](https://github.com/zeromq/clrzmq4) (or the release ZeroMQ.dll).

Now compile the project and run `./ZGuideExamples.exe` !

```
Usage: ./ZGuideExamples.exe [--option=++] [--option=tcp://192.168.1.1:8080] <command> World Me You
```

#### HWServer, HWClient

```
	Usage: ./ZGuideExamples.exe [HWClient](https://github.com/metadings/zguide/blob/master/examples/C%23/hwclient.cs)

	Usage: ./ZGuideExamples.exe [HWServer](https://github.com/metadings/zguide/blob/master/examples/C%23/hwserver.cs) [Name]

        Name   Your name. Default: World
```

#### Version

```
	Usage: ./ZGuideExamples.exe Version
```

#### WUServer, WUClient, WUProxy

```
	Usage: ./ZGuideExamples.exe [WUServer](https://github.com/metadings/zguide/blob/master/examples/C%23/wuserver.cs)

	Usage: ./ZGuideExamples.exe [WUClient](https://github.com/metadings/zguide/blob/master/examples/C%23/wuclient.cs) [ZipCode] [Endpoint]

        ZipCode   The zip code to subscribe. Default is 72622 Nürtingen
	    Endpoint  Where WUClient should connect to.
	              Default is tcp://127.0.0.1:5556

	Usage: ./ZGuideExamples.exe [WUProxy](https://github.com/metadings/zguide/blob/master/examples/C%23/wuproxy.cs)
```

#### TaskVent, TaskWork, TaskSink

```
	Usage: ./ZGuideExamples.exe [TaskVent](https://github.com/metadings/zguide/blob/master/examples/C%23/taskvent.cs)

	Usage: ./ZGuideExamples.exe [TaskWork](https://github.com/metadings/zguide/blob/master/examples/C%23/taskwork.cs)

	Usage: ./ZGuideExamples.exe [TaskSink](https://github.com/metadings/zguide/blob/master/examples/C%23/tasksink.cs)
```

#### MSReader, MSPoller

```
	Usage: ./ZGuideExamples.exe [MSReader](https://github.com/metadings/zguide/blob/master/examples/C%23/msreader.cs)

	Usage: ./ZGuideExamples.exe [MSPoller](https://github.com/metadings/zguide/blob/master/examples/C%23/mspoller.cs)
```

#### RRClient, RRWorker, RRBroker

```
	Usage: ./ZGuideExamples.exe [RRClient](https://github.com/metadings/zguide/blob/master/examples/C%23/rrclient.cs)

	Usage: ./ZGuideExamples.exe [RRWorker](https://github.com/metadings/zguide/blob/master/examples/C%23/rrworker.cs) [Name] [Endpoint]

        Name      Your Name
	    Endpoint  Where RRClient should connect to.
	              Default is tcp://127.0.0.1:5559

	Usage: ./ZGuideExamples.exe [RRBroker](https://github.com/metadings/zguide/blob/master/examples/C%23/rrbroker.cs)
```

#### MsgQueue

```
	Usage: ./ZGuideExamples.exe [MsgQueue](https://github.com/metadings/zguide/blob/master/examples/C%23/msgqueue.cs)
```

#### TaskWork2, TaskSink2

Use with TaskVent.

```
	Usage: ./ZGuideExamples.exe [TaskWork2](https://github.com/metadings/zguide/blob/master/examples/C%23/taskwork2.cs)

	Usage: ./ZGuideExamples.exe [TaskSink2](https://github.com/metadings/zguide/blob/master/examples/C%23/tasksink2.cs)
```

#### Interrupt

Use with HWClient.

```
	Usage: ./ZGuideExamples.exe [Interrupt](https://github.com/metadings/zguide/blob/master/examples/C%23/interrupt.cs)
```

#### MTServer, MTRelay

```
	Usage: ./ZGuideExamples.exe [MTServer](https://github.com/metadings/zguide/blob/master/examples/C%23/mtserver.cs)

	Usage: ./ZGuideExamples.exe [MTRelay](https://github.com/metadings/zguide/blob/master/examples/C%23/mtrelay.cs)
```

#### SyncPub, SyncSub

```
	Usage: ./ZGuideExamples.exe [SyncPub](https://github.com/metadings/zguide/blob/master/examples/C%23/syncpub.cs)

	Usage: ./ZGuideExamples.exe [SyncSub](https://github.com/metadings/zguide/blob/master/examples/C%23/syncsub.cs)
```

#### PSEnvPub, PSEnvSub

```
	Usage: ./ZGuideExamples.exe [PSEnvPub](https://github.com/metadings/zguide/blob/master/examples/C%23/psenvpub.cs)

	Usage: ./ZGuideExamples.exe [PSEnvSub](https://github.com/metadings/zguide/blob/master/examples/C%23/psenvsub.cs)
```

#### Identity

```
	Usage: ./ZGuideExamples.exe [Identity](https://github.com/metadings/zguide/blob/master/examples/C%23/identity.cs)
```

#### RTReq, RTDealer

```
	Usage: ./ZGuideExamples.exe [RTReq](https://github.com/metadings/zguide/blob/master/examples/C%23/rtreq.cs)

	Usage: ./ZGuideExamples.exe [RTDealer](https://github.com/metadings/zguide/blob/master/examples/C%23/rtdealer.cs)
```

#### LBBroker

```
	Usage: ./ZGuideExamples.exe [LBBroker](https://github.com/metadings/zguide/blob/master/examples/C%23/lbbroker.cs)
```

#### AsyncSrv

```
	Usage: ./ZGuideExamples.exe [AsyncSrv](https://github.com/metadings/zguide/blob/master/examples/C%23/asyncsrv.cs)
```

#### Peering1, Peering2

```
	Usage: ./ZGuideExamples.exe [Peering1](https://github.com/metadings/zguide/blob/master/examples/C%23/peering1.cs) World Receiver0
				                Peering1 Receiver0 World

	Usage: ./ZGuideExamples.exe [Peering2](https://github.com/metadings/zguide/blob/master/examples/C%23/peering2.cs)
```

#### LPClient, LPServer

```
	Usage: ./ZGuideExamples.exe [LPClient](https://github.com/metadings/zguide/blob/master/examples/C%23/lpclient.cs) [Name]

        Name   Your name. Default: World

	Usage: ./ZGuideExamples.exe [LPServer](https://github.com/metadings/zguide/blob/master/examples/C%23/lpserver.cs)
```

#### SPQueue, SPWorker

```
	Usage: ./ZGuideExamples.exe [SPQueue](https://github.com/metadings/zguide/blob/master/examples/C%23/spqueue.cs)

	Usage: ./ZGuideExamples.exe [SPWorker](https://github.com/metadings/zguide/blob/master/examples/C%23/spworker.cs)
```

#### PPQueue, PPWorker

```
	Usage: ./ZGuideExamples.exe [PPQueue](https://github.com/metadings/zguide/blob/master/examples/C%23/ppqueue.cs)

	Usage: ./ZGuideExamples.exe [PPWorker](https://github.com/metadings/zguide/blob/master/examples/C%23/ppworker.cs) [Name]

        Name   Your name. Default: World
```

#### FLServer1, FLClient1

```
	Usage: ./ZGuideExamples.exe [FLServer1](https://github.com/metadings/zguide/blob/master/examples/C%23/flserver1.cs) [Endpoint]

	    Endpoint  Where FLServer1 should bind on.
	              Default is tcp://127.0.0.1:7780

	Usage: ./ZGuideExamples.exe [FLClient1](https://github.com/metadings/zguide/blob/master/examples/C%23/flclient1.cs) [Endpoint]

	    Endpoint  Where FLClient1 should connect to.
	              Default is tcp://127.0.0.1:7780
```

#### FLServer2, FLClient2

```
	Usage: ./ZGuideExamples.exe [FLServer2](https://github.com/metadings/zguide/blob/master/examples/C%23/flserver2.cs) [Endpoint]

	    Endpoint  Where FLServer2 should bind on.
	              Default is tcp://127.0.0.1:7780

	Usage: ./ZGuideExamples.exe [FLClient2](https://github.com/metadings/zguide/blob/master/examples/C%23/flclient2.cs) [Endpoint] ...

	    Endpoint  Where FLClient2 should connect to.
	              Default is tcp://127.0.0.1:7780
```

#### FLServer3, FLClient3, FLCliApi

Have a look on the [FLCliApi.FreelanceClient](https://github.com/metadings/zguide/blob/master/examples/C%23/flcliapi.cs)!

```
	Usage: ./ZGuideExamples.exe [FLServer3](https://github.com/metadings/zguide/blob/master/examples/C%23/flserver3.cs)

	Usage: ./ZGuideExamples.exe [FLClient3](https://github.com/metadings/zguide/blob/master/examples/C%23/flclient3.cs) [Name] [Endpoint]

	    Name      Your Name
	    Endpoint  Where FLClient3 should connect to.
	              Default: tcp://127.0.0.1:5555
```

#### Espresso

```
	Usage: ./ZGuideExamples.exe [Espresso](https://github.com/metadings/zguide/blob/master/examples/C%23/espresso.cs)
```

#### PathoPub, PathoSub, LVCache

```
	Usage: ./ZGuideExamples.exe [PathoPub](https://github.com/metadings/zguide/blob/master/examples/C%23/pathopub.cs) [Endpoint]

	    Endpoint  Where PathoPub should connect to.
	              Default is null, Binding on tcp://*:5556

	Usage: ./ZGuideExamples.exe [PathoSub](https://github.com/metadings/zguide/blob/master/examples/C%23/pathosub.cs) [Endpoint]

	    Endpoint  Where PathoSub should connect to.
	              Default is tcp://127.0.0.1:5556

	Usage: ./ZGuideExamples.exe [LVCache](https://github.com/metadings/zguide/blob/master/examples/C%23/lvcache.cs)

        The LVCache is binding on tcp://*:5557 and tcp://*.5558
```

#### SuiSnail

```
	Usage: ./ZGuideExamples.exe [SuiSnail](https://github.com/metadings/zguide/blob/master/examples/C%23/suisnail.cs)
```

#### EAgain

```
	Usage: ./ZGuideExamples.exe [EAgain](https://github.com/metadings/zguide/blob/master/examples/C%23/eagain.cs)
```

