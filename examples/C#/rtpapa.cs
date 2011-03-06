//
//  Custom routing Router to Papa (XREP to REP)
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using ZMQ;

namespace rtpapa {
    class Program {
        //  We will do this all in one thread to emphasize the sequence
        //  of events...
        static void Main(string[] args) {
            using (Context ctx = new Context(1)) {
                using (Socket client = ctx.Socket(SocketType.XREP),
                    worker = ctx.Socket(SocketType.REP)) {

                    client.Bind("inproc://routing");
                    worker.StringToIdentity("A", Encoding.Unicode);
                    worker.Connect("inproc://routing");

                    //  Wait for the worker to connect so that when we send a message
                    //  with routing envelope, it will actually match the worker...
                    Thread.Sleep(1000);

                    //  Send papa address, address stack, empty part, and request
                    client.SendMore("A", Encoding.Unicode);
                    client.SendMore("address 3", Encoding.Unicode);
                    client.SendMore("address 2", Encoding.Unicode);
                    client.SendMore("address 1", Encoding.Unicode);
                    client.SendMore("", Encoding.Unicode);
                    client.Send("This is the workload", Encoding.Unicode);

                    //  Worker should get just the workload
                    ZHelpers.Dump(worker, Encoding.Unicode);

                    //  We don't play with envelopes in the worker
                    worker.Send("This is the reply", Encoding.Unicode);

                    //  Now dump what we got off the XREP socket...
                    ZHelpers.Dump(client, Encoding.Unicode);
                }
            }
        }
    }
}
