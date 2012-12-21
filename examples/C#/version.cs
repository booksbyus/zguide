//
//  Report 0MQ version
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using ZeroMQ;
using zguide;

namespace ZMQGuide 
{
    internal class Program1 
    {
        public static void Main(string[] args) 
        {
            Console.WriteLine(ZHelpers.Version());
        }
    }
}
