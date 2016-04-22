using System;
using System.Linq;
using System.Threading;
using ZeroMQ;

namespace Examples
{
    using MDCliApi2; // Let us build this source without creating a library
    static partial class Program
    {
        //  Majordomo Protocol client example
        //  Uses the mdcli API to hide all MDP aspects
        public static void MDClient2(string[] args)
        {
            CancellationTokenSource cts = new CancellationTokenSource();
            Console.CancelKeyPress += (s, ea) =>
            {
                ea.Cancel = true;
                cts.Cancel();
            };

            using (MajordomoClient session = new MajordomoClient("tcp://127.0.0.1:5555", Verbose))
            {
                int count;
                for (count = 0; count < 100000 && !cts.IsCancellationRequested; count++)
                {
                    ZMessage request = new ZMessage();
                    request.Prepend(new ZFrame("Hello world"));
                    session.Send("echo", request, cts);
                }
                for (count = 0; count < 100000 && !cts.IsCancellationRequested; count++)
                {
                    using (ZMessage reply = session.Recv(cts))
                        if (reply == null)
                            break; // Interrupt or failure
                }
                Console.WriteLine("{0} replies received\n", count);
            }
        }
    }
}
