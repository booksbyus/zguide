﻿//
//  Synchronized publisher
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
using System.Threading;
using ZeroMQ;

namespace zguide.syncpub
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket publisher = context.CreateSocket(SocketType.PUB), syncService = context.CreateSocket(SocketType.REP))
                {
                    publisher.Bind("tcp://*:5561");
                    syncService.Bind("tcp://*:5562");

                    //  Get synchronization from subscribers
                    const int subscribersToWaitFor = 10;
                    for (int count = 0; count < subscribersToWaitFor; count++)
                    {
                        syncService.Receive(Encoding.Unicode);
                        syncService.Send("", Encoding.Unicode);
                    }

                    //  Now broadcast exactly 1M updates followed by END
                    const int updatesToSend = 1000000;
                    for (int updateId = 0; updateId < updatesToSend; updateId++)
                    {
                        publisher.Send("Rhubard", Encoding.Unicode);
                    }

                    publisher.Send("END", Encoding.Unicode);
                }
            }
        }
    }
}
