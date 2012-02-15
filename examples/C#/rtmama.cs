//
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
using ZMQ;
using System.Threading;

namespace ZMQGuide
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            const int workersCount = 10;
            var workers = new List<Thread>(workersCount);
            
            using (var context = new Context(1))
            {
                using (Socket client = context.Socket(SocketType.ROUTER))
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
                        string address = client.Recv(Encoding.Unicode);
                        string empty = client.Recv(Encoding.Unicode);
                        string ready = client.Recv(Encoding.Unicode);

                        client.SendMore(address, Encoding.Unicode);
                        client.SendMore();
                        client.Send("This is the workload", Encoding.Unicode);
                    }

                    //  Now ask mamas to shut down and report their results
                    for (int taskNbr = 0; taskNbr < workersCount; taskNbr++)
                    {
                        string address = client.Recv(Encoding.Unicode);
                        string empty = client.Recv(Encoding.Unicode);
                        string ready = client.Recv(Encoding.Unicode);

                        client.SendMore(address, Encoding.Unicode);
                        client.SendMore();
                        client.Send("END", Encoding.Unicode);
                    }
                }
            }

            Console.ReadLine();
        }

        public static void WorkerTask()
        {
            var randomizer = new Random(DateTime.Now.Millisecond);

            using (var context = new Context(1))
            {
                using (Socket worker = context.Socket(SocketType.REQ))
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
                        string workload = worker.Recv(Encoding.Unicode);

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

                    Console.WriteLine("ID ({0}) processed: {1} tasks", worker.IdentityToString(Encoding.Unicode), total);
                }
            }
        }
    }
}
