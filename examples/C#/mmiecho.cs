using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.Remoting.Messaging;
using System.Text;
using System.Threading;
using ZeroMQ;

namespace Examples
{
    //  Lets us build this source without creating a library
    using MDCliApi;

    static partial class Program
    {
        //  MMI echo query example
        public static void MMIEcho(string[] args)
        {
            bool verbose = (args.Any(e => e.ToLower().Equals("-v")
                                          || e.ToLower().Equals("--verbose")));
            Console.WriteLine("Verbose: {0}", verbose);

            CancellationTokenSource cancellor = new CancellationTokenSource();
            Console.CancelKeyPress += (s, ea) =>
            {
                ea.Cancel = true;
                cancellor.Cancel();
            };

            using (MajordomoClient session = new MajordomoClient("tcp://localhost:5555", verbose))
            {
                ZMessage request  = new ZMessage();
                request.Add(new ZFrame("echo"));

                ZMessage reply = session.Send("mmi.service", request, cancellor);
                if (reply != null)
                {
                    var replycode = reply[0].ToString();
                    "Loopup echo service: {0}\n".DumpString(replycode);
                    reply.Dispose();
                }
                else
                    "E: no response from broker, make sure it's running\n".DumpString();
            }
        }
    }
}
