//
//  Custom routing Router to Dealer
//
//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each thread has its own
//  context and conceptually acts as a separate process.
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk. ptomasroos@gmail.com

using System;
using System.Threading;
using System.Collections.Generic;
using System.Text;
using ZMQ;

namespace ZMQGuide
{
    internal class Program
    {
        //  We have two workers, here we copy the code, normally these would
        //  run on different boxes...
        public static void Main(string[] args)
        {
            var randomizer = new Random(DateTime.Now.Millisecond);
            var workers = new List<Thread>(new[] { new Thread(WorkerTaskA), new Thread(WorkerTaskB) });
            
            using (var context = new Context(1))
            {
                using (Socket client = context.Socket(SocketType.ROUTER))
                {
                    client.Bind("tcp://*:5555");
                    foreach (Thread thread in workers)
                    {
                        thread.Start();
                    }

                    //  Wait for threads to connect, since otherwise the messages we send won't be routable.
                    Thread.Sleep(1000);

                    for (int taskNumber = 0; taskNumber < 10; taskNumber++)
                    {
                        //  Send two message parts, first the address...
                        client.SendMore(randomizer.Next(3) > 0 ? "A" : "B", Encoding.Unicode);

                        //  And then the workload
                        client.Send("This is the workload", Encoding.Unicode);
                    }

                    client.SendMore("A", Encoding.Unicode);
                    client.Send("END", Encoding.Unicode);

                    client.SendMore("B", Encoding.Unicode);
                    client.Send("END", Encoding.Unicode);
                }
            }

            Console.ReadKey();
        }

        private static void WorkerTaskA()
        {
            using (var context = new Context(1))
            {
                using (Socket worker = context.Socket(SocketType.DEALER))
                {
                    worker.StringToIdentity("A", Encoding.Unicode);
                    worker.Connect("tcp://localhost:5555");

                    int total = 0;

                    bool end = false;

                    while (!end)
                    {
                        string request = worker.Recv(Encoding.Unicode);
                        
                        if (request.Equals("END"))
                        {
                            end = true;
                        }
                        else
                        {
                            total++;
                        }
                    }

                    Console.WriteLine("A Received: {0}", total);
                }
            }
        }

        private static void WorkerTaskB()
        {
            using (var context = new Context(1))
            {
                using (Socket worker = context.Socket(SocketType.DEALER))
                {
                    worker.StringToIdentity("B", Encoding.Unicode);
                    worker.Connect("tcp://localhost:5555");

                    int total = 0;

                    bool end = false;

                    while (!end)
                    {
                        string request = worker.Recv(Encoding.Unicode);

                        if (request.Equals("END"))
                        {
                            end = true;
                        }
                        else
                        {
                            total++;
                        }
                    }

                    Console.WriteLine("B Received: {0}", total);
                }
            }
        }
    }
}
