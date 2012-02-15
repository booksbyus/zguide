//
//  Weather update client
//  Connects SUB socket to tcp://localhost:5556
//  Collects weather updates and finds avg temp in zipcode
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
using ZMQ;

namespace ZMQGuide 
{
    internal class Program 
    {
        public static void Main(string[] args) {
            Console.WriteLine("Collecting updates from weather server...");

            // default zipcode is 10001
            string zipcode = "10001 "; // the reason for having a space after 10001 is in case of the message would start with 100012 which we are not interested in

            if (args.Length > 0)
                zipcode = args[1] + " ";

            using (var context = new Context(1))
            {
                using (Socket subscriber = context.Socket(SocketType.SUB))
                {
                    subscriber.Subscribe(zipcode, Encoding.Unicode);
                    subscriber.Connect("tcp://localhost:5556");

                    const int updatesToCollect = 100;
                    int totalTemperature = 0;

                    for (int updateNumber = 0; updateNumber < updatesToCollect; updateNumber++)
                    {
                        string update = subscriber.Recv(Encoding.Unicode);
                        totalTemperature += Convert.ToInt32(update.Split()[1]);
                    }

                    Console.WriteLine("Average temperature for zipcode {0} was {1}F", zipcode, totalTemperature / updatesToCollect);
                }
            }
        }
    }
}
