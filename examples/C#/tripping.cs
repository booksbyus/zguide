using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.Remoting.Messaging;
using System.Text;
using System.Threading;
using ZeroMQ;

namespace Examples
{
    static partial class Program
    {
        //  Round-trip demonstrator
        //  While this example runs in a single process, that is just to make
        //  it easier to start and stop the example. The client task signals to
        //  main when it's ready.
        public static void Tripping(string[] args)
        {
            CancellationTokenSource cancellor = new CancellationTokenSource();
            Console.CancelKeyPress += (s, ea) =>
            {
                ea.Cancel = true;
                cancellor.Cancel();
            };

            using (ZContext ctx = new ZContext())
            {
                using (var client = new ZActor(ctx, Tripping_ClientTask))
                {
                    (new Thread(() => Tripping_WorkerTask(ctx))).Start();
                    (new Thread(() => Tripping_BrokerTask(ctx))).Start();
                    client.Start();
                    using (var signal = client.Frontend.ReceiveFrame())
                        if (Verbose)
                            signal.ToString().DumpString();
                }
            }
        }



        static void Tripping_ClientTask(ZContext ctx, ZSocket pipe, CancellationTokenSource cancellor, object[] args)
        {
            using (ZSocket client = new ZSocket(ctx, ZSocketType.DEALER))
            {
                client.Connect("tcp://127.0.0.1:5555");
                "Setting up test...".DumpString();
                Thread.Sleep(100);

                int requests;
                "Synchronous round-trip test...".DumpString();
                var start = DateTime.Now;
                Stopwatch sw = Stopwatch.StartNew();
                for (requests = 0; requests < 10000; requests++)
                {
                    using (var outgoing = new ZFrame("hello"))
                    {
                        client.Send(outgoing);
                        using (var reply = client.ReceiveFrame())
                        {
                            if (Verbose)
                                reply.ToString().DumpString();
                        }
                    }
                }
                sw.Stop();
                " {0} calls - {1} ms => {2} calls / second".DumpString(requests, sw.ElapsedMilliseconds, requests * 1000 / sw.ElapsedMilliseconds);

                "Asynchronous round-trip test...".DumpString();
                sw.Restart();
                for (requests = 0; requests < 100000; requests++)
                    using (var outgoing = new ZFrame("hello"))
                        client.SendFrame(outgoing);

                for (requests = 0; requests < 100000; requests++)
                    using (var reply = client.ReceiveFrame())
                        if (Verbose)
                            reply.ToString().DumpString();
                sw.Stop();
                " {0} calls - {1} ms => {2} calls / second".DumpString(requests, sw.ElapsedMilliseconds, requests * 1000 / sw.ElapsedMilliseconds);
                using (var outgoing = new ZFrame("done"))
                    pipe.SendFrame(outgoing);
            }
        }

        //  .split worker task
        //  Here is the worker task. All it does is receive a message, and
        //  bounce it back the way it came:
        static void Tripping_WorkerTask(ZContext ctx)
        {
            using (var worker = new ZSocket(ctx, ZSocketType.DEALER))
            {
                worker.Connect("tcp://127.0.0.1:5556");

                while (true)
                {
                    ZError error;
                    ZMessage msg = worker.ReceiveMessage(out error);
                    if (error == null && worker.Send(msg, out error))
                        continue;
                    // errorhandling, context terminated or sth else
                    if (error.Equals(ZError.ETERM))
                        return; // Interrupted
                    throw new ZException(error);
                }
            }
        }

        //  .split broker task
        //  Here is the broker task. It uses the {{zmq_proxy}} function to switch
        //  messages between frontend and backend:
        static void Tripping_BrokerTask(ZContext ctx)
        {
            using (var frontend = new ZSocket(ctx, ZSocketType.DEALER))
            using (var backend = new ZSocket(ctx, ZSocketType.DEALER))
            {
                frontend.Bind("tcp://*:5555");
                backend.Bind("tcp://*:5556");

                ZError error;
                if (!ZContext.Proxy(frontend, backend, out error))
                {
                    if (Equals(error, ZError.ETERM))
                        return; // Interrupted
                    throw new ZException(error);
                }
            }
        }
    }
}
