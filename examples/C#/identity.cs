//
//  Demonstrate identities as used by the request-reply pattern.
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System.Text;
using ZMQ;

namespace ZMQGuide
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = new Context())
            {
                using (Socket sink = context.Socket(SocketType.ROUTER), anonymous = context.Socket(SocketType.REQ), identified = context.Socket(SocketType.REQ))
                {
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
