﻿//
//  Custom routing Router to Mama (ROUTER to REQ)
//
//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each thread has its own
//  context and conceptually acts as a separate process.
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Collections.Generic;
using System.Text;
using ZeroMQ;
using System.Threading;
using zguide;

namespace zguide.rtreq
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            const int workersCount = 10;
            var workers = new List<Thread>(workersCount);
            
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket client = context.CreateSocket(SocketType.ROUTER))
                {
                    client.Bind("tcp://*:5555");

                    for (int workerNumber = 0; workerNumber < workersCount; workerNumber++)
                    {
                        workers.Add(new Thread(WorkerTask));
                        workers[workerNumber].Start();
                    }

                    for (int taskNumber = 0; taskNumber < workersCount * 10; taskNumber++)
                    {
                        //  LRU worker is next waiting in queue
                        string address = client.Receive(Encoding.Unicode);
                        string empty = client.Receive(Encoding.Unicode);
                        string ready = client.Receive(Encoding.Unicode);

                        client.SendMore(address, Encoding.Unicode);
                        client.SendMore(string.Empty, Encoding.Unicode);
                        client.Send("This is the workload", Encoding.Unicode);
                    }

                    //  Now ask mamas to shut down and report their results
                    for (int taskNbr = 0; taskNbr < workersCount; taskNbr++)
                    {
                        string address = client.Receive(Encoding.Unicode);
                        string empty = client.Receive(Encoding.Unicode);
                        string ready = client.Receive(Encoding.Unicode);

                        client.SendMore(address, Encoding.Unicode);
                        client.SendMore(string.Empty, Encoding.Unicode);
                        client.Send("END", Encoding.Unicode);
                    }
                }
            }

            Console.ReadLine();
        }

        public static void WorkerTask()
        {
            var randomizer = new Random(DateTime.Now.Millisecond);

            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket worker = context.CreateSocket(SocketType.REQ))
                {
                    //  We use a string identity for ease here
                    ZHelpers.SetID(worker, Encoding.Unicode);
                    worker.Connect("tcp://localhost:5555");

                    int total = 0;

                    bool end = false;
                    while (!end)
                    {
                        //  Tell the router we're ready for work
                        worker.Send("Ready", Encoding.Unicode);

                        //  Get workload from router, until finished
                        string workload = worker.Receive(Encoding.Unicode);

                        if (workload.Equals("END"))
                        {
                            end = true;
                        }
                        else
                        {
                            total++;

                            Thread.Sleep(randomizer.Next(1, 1000)); //  Simulate 'work'
                        }
                    }

                    Console.WriteLine("ID ({0}) processed: {1} tasks", worker.Identity, total);
                }
            }
        }
    }
}
