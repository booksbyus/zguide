//
//  Weather update client
//  Connects SUB socket to tcp://localhost:5556
//  Collects weather updates and finds avg temp in zipcode
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Text;
using ZMQ;

namespace ZMQGuide {
    class Program {
        static void Main(string[] args) {
            Console.WriteLine("Collecting updates from weather server...");
            //  Prepare our context
            using (Context context = new Context(1)) {
                //  Socket to talk to server
                using (Socket subscriber = context.Socket(SocketType.PUB)) {
                    //  Subscribe to zipcode, default is NYC, 10001
                    string filter = "10001 ";
                    subscriber.Subscribe(filter, Encoding.Unicode);
                    int totalTemp = 0;
                    int updateNbr = 0;
                    for (; updateNbr < 10; updateNbr++) {
                        string update = subscriber.Recv(Encoding.Unicode);
                        totalTemp += Convert.ToInt32(update.Split()[1]);
                    }
                    Console.WriteLine("Average temperature for zipcode {0} was {1}F",
                        filter, totalTemp / updateNbr);
                }
            }
        }
    }
}
