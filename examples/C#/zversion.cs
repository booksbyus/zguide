//
//  Report 0MQ version
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using ZMQ;

namespace ZMQGuide {
    class Program {
        static void Main(string[] args) {
            Console.WriteLine(ZHelpers.Version());
        }
    }
}
