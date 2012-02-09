//
//  Report 0MQ version
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using ZMQ;

namespace ZMQGuide 
{
    internal class Program 
    {
        public static void Main(string[] args) 
        {
            Console.WriteLine(ZHelpers.Version());
        }
    }
}
