﻿//
//  Report 0MQ version
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using ZeroMQ;
using zguide;

namespace zguide.version
{
    internal class Program 
    {
        public static void Main(string[] args) 
        {
            Console.WriteLine(ZHelpers.Version());
        }
    }
}
