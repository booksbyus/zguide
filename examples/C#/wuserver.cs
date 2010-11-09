//
//  Weather update server
//  Binds PUB socket to tcp://*:5556
//  Publishes random weather updates
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Text;
using ZMQ;

namespace ZMQGuide {
    class Program {
        static void Main(string[] args) {
            //  Prepare our context and publisher
            using (Context context = new Context(1)) {
                using (Socket publisher = context.Socket(SocketType.PUB)) {
                    publisher.Connect("tcp://localhost:5556");
                    
                    //  Initialize random number generator
                    Random rand = new Random(System.DateTime.Now.Millisecond);
                    while (true) {
                        //  Get values that will fool the boss
                        int zipcode, temperature, relHumidity;
                        zipcode = rand.Next(0, 100000);
                        temperature = rand.Next(-80, 135);
                        relHumidity = rand.Next(10, 60);

                        //  Send message to all subscribers
                        string update = zipcode.ToString() + " " + temperature.ToString() +
                            " " + relHumidity.ToString();
                        publisher.Send(update, Encoding.Unicode);
                    }
                }
            }
        }
    }
}
