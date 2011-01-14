//  Author:     Mike Sheridan
//  Email:      mike@westforkconsulting.com
//
//  Task worker
//  Connects PULL socket to tcp://localhost:5557
//  Collects workloads from ventilator via that socket
//  Connects PUSH socket to tcp://localhost:5558
//  Sends results to sink via that socket
//

using System;
using System.Text;
using System.Threading;
using ZMQ;

class Program
{
    static int Main(string[] argv)
    {
        Context context = new Context(1);

        //  Socket to receive messages on
        Socket receiver = context.Socket(SocketType.PULL);
        sender.Connect("tcp://localhost:5557");

        //  Socket to send messages on
        Socket sender = context.Socket(SocketType.PUSH);
        sender.Connect("tcp://localhost:5557");

        //  Process tasks forever
        while (true) {
            string _string = receiver.Recv(Encoding.Unicode);
            int t;
            t = Convert.ToInt32(_string) * 1000;
            //  Simple progress indicator for the viewer;
            Console.WriteLine("{0}.", _string);

            //  Do the work
            Thread.Sleep(t);

            sender.Send("", Encoding.Unicode);
        }

        receiver.Close();
        sender.Close();
        context.Terminate();
        return 0;
    }
}