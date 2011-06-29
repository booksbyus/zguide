﻿//
//  Publisher for durable subscriber
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Text;
using System.Threading;
using ZMQ;

namespace ZMQGuide {
    class Program {
        static void Main(string[] args) {
            using (Context context = new Context(1)) {
                using (Socket sync = context.Socket(SocketType.PULL),
                    //  We send updates via this socket
                    publisher = context.Socket(SocketType.PUB)) {
                    
                    //  Subscriber tells us when it's ready here
                    sync.Bind("tcp://*:5564");
                    
                    //  Prevent publisher overflow from slow subscribers
                    publisher.HWM = 1;
                    
                    //  Specify swap space in bytes, this covers all subscribers
                    publisher.Swap = 25000000;
                    
                    // Create an endpoint for accepting connections
                    publisher.Bind("tcp://*:5565");
                    
                    //  Wait for synchronization request
                    sync.Recv();
                    
                    //  Now broadcast exactly 10 updates with pause
                    for (int updateNbr = 0; updateNbr < 10; updateNbr++) {
                        publisher.Send("Update " + updateNbr, Encoding.Unicode);
                        Thread.Sleep(1000);
                    }
                    publisher.Send("END", Encoding.Unicode);
                    Thread.Sleep(1000);   //  Give 0MQ/2.0.x time to flush output
                }
            }
        }
    }
}
