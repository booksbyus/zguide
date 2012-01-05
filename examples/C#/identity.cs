//
//  Demonstrate identities as used by the request-reply pattern.
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk
using System;
using System.Collections.Generic;
using System.Text;
using ZMQ;

namespace identity {
    class Program {
        static void Main(string[] args) {
            using (Context ctx = new Context()) {
                using (Socket sink = ctx.Socket(SocketType.ROUTER),
                    anonymous = ctx.Socket(SocketType.REQ),
                    identified = ctx.Socket(SocketType.REQ)) {

                    sink.Bind("inproc://example");

                    //  First allow 0MQ to set the identity
                    anonymous.Connect("inproc://example");
                    anonymous.Send("ROUTER uses a generated UUID", Encoding.Unicode);
                    ZHelpers.Dump(sink, Encoding.Unicode);

                    //  Then set the identity ourself
                    identified.StringToIdentity("Hello", Encoding.Unicode);
                    identified.Connect("inproc://example");
                    identified.Send("ROUTER socket uses REQ's socket identity", Encoding.Unicode);
                    ZHelpers.Dump(sink, Encoding.Unicode);
                }
            }
        }
    }
}
