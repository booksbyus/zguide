using System;
using System.Text;

/**
* Author: Eric Desgranges
* Email: eric@vcardprocessor.com
* License: This example code licensed under the MIT/X11 license.
*/

namespace Client
{
    public class Program
    {
        //
        //  Weather update client
        //  Connects SUB socket to tcp://localhost:5556
        //  Collects weather updates and finds avg temp in zipcode
        //
        public static void Main()
        {
            using (var context = new ZMQ.Context(1))
            {
                //  Socket to talk to server
                Console.WriteLine("Collecting updates from weather server...");
                var subscriber = context.Socket(ZMQ.SUB);
                subscriber.Connect("tcp://localhost:5556");

                //  Subscribe to zipcode NYC, 10001
                string filter = "10001";
                subscriber.SetSockOpt(ZMQ.SUBSCRIBE, filter);

                //  Process 100 updates
                int update_nbr;
                long total_temp = 0;
                for (update_nbr = 0; update_nbr < 100; update_nbr++)
                {
                    byte[] bytes;
                    subscriber.Recv(out bytes);
                    string astring = Encoding.ASCII.GetString(bytes);
                    string[] result = astring.Split(new char[] { ' ' });
                    int zipcode, temperature, relhumidity;
                    int.TryParse(result[0], out zipcode);
                    int.TryParse(result[1], out temperature);
                    int.TryParse(result[0], out relhumidity);
                    total_temp += temperature;
                }
                Console.WriteLine("Average temperature for zipcode {0} was {1}F", filter, total_temp / update_nbr);
            }
        }
    }
}