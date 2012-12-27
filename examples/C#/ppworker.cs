//
//  Paranoid Pirate Worker
//
//  Author:     Pepper Garretson
//  Email:      jpgarretson@gmail.com
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using ZeroMQ;
using zguide;

namespace zguide.ppworker 
{
    class Program
    {
        private const int HEARTBEAT_LIVENESS = 3; // 3-5 is reasonable
        private const int HEARTBEAT_INTERVAL = 1000; // msecs
        private const int INTERVAL_INIT = 1000;  // Initial reconnect
        private const int INTERVAL_MAX = 32000;  // After exponential backoff

        private const string PPP_READY = "READY";
        private const string PPP_HEARTBEAT = "HEARTBEAT";

        static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                int interval = INTERVAL_INIT;
                int liveness = HEARTBEAT_LIVENESS;

                while (true)
                {
                    int cylces = 0;

                    using (ZmqSocket worker = ConnectWorker(context))
                    {
                        worker.ReceiveReady += (socket, revents) =>
                        {
                            var zmsg = new ZMessage(revents.Socket);

                            byte[] identity = zmsg.Unwrap();

                            switch (Encoding.Unicode.GetString(zmsg.Address))
                            {
                                case PPP_HEARTBEAT:
                                    interval = INTERVAL_INIT;
                                    liveness = HEARTBEAT_LIVENESS;
                                    break;
                                default:
                                    if (zmsg.FrameCount > 1)
                                    {
                                        if (!doTheWork(cylces++))
                                        {
                                            break;
                                        }
                                        interval = INTERVAL_INIT;
                                        liveness = HEARTBEAT_LIVENESS;
                                        Console.WriteLine("W: work completed");
                                        zmsg.Send(worker);
                                    }
                                    else
                                    {
                                        Console.WriteLine("E: invalied message {0}", identity);
                                    }
                                    break;
                            };
                        };

                        //Send out heartbeats at regular intervals
                        DateTime heartbeat_at = DateTime.Now.AddMilliseconds(HEARTBEAT_INTERVAL);

                        while (true)
                        {
                            var poller = new Poller(new List<ZmqSocket> { worker });
                            
                            poller.Poll(TimeSpan.FromMilliseconds(HEARTBEAT_INTERVAL * 1000));

                            //If liveness hits zero, queue is considered disconnected
                            if (--liveness <= 0)
                            {
                                Console.WriteLine("W: heartbeat failure, can't reach queue.");
                                Console.WriteLine("W: reconnecting in {0} msecs...", interval);

                                try
                                {
                                    Thread.Sleep(interval);
                                }
                                catch (System.Exception ex)
                                {
                                    Console.WriteLine(ex.Message);
                                }

                                //Exponential Backoff
                                if (interval < INTERVAL_MAX)
                                    interval *= 2;

                                liveness = HEARTBEAT_LIVENESS;

                                //Break the while loop and start the connection over
                                break;
                            }

                            //Send heartbeat to queue if it's time
                            if (DateTime.Now > heartbeat_at)
                            {
                                heartbeat_at = DateTime.Now.AddMilliseconds(HEARTBEAT_INTERVAL);
                                ZMessage zmsg = new ZMessage(PPP_HEARTBEAT);
                                zmsg.Send(worker);
                            }
                        }
                    }
                }
            }
        }


        /*
         * Do the job, simuate problems if cycle > 5
         */
        static bool doTheWork(int cycle)
        {
            Random rand = new Random();

            try
            {
                if (cycle > 3 && rand.Next(6) == 0)
                {
                    Console.WriteLine("I: simulating a crash");
                    return false;
                }
                else if (cycle > 3 && rand.Next(6) == 0)
                {
                    Console.WriteLine("I: simulating a CPU overload");
                    Thread.Sleep(3000);
                }

                //Do some work
                Thread.Sleep(300);
            }
            catch(System.Exception ex) 
            {
                Console.WriteLine(ex.Message);
            }

            return true;
        }

        static ZmqSocket ConnectWorker(ZmqContext context)
        {
            ZmqSocket worker = context.CreateSocket(SocketType.DEALER);

            //Set random identity to make tracing easier
            ZHelpers.SetID(worker, Encoding.Unicode);
            worker.Connect("tcp://localhost:5556");

            //Tell the queue we're ready for work
            Console.WriteLine("I: worker ready");
            worker.Send(PPP_READY, Encoding.Unicode);

            return worker;
        }
    }
}
