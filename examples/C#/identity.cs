//
//  Demonstrate identities as used by the request-reply pattern.
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System.Text;
using ZeroMQ;
using zguide;

namespace zguide.identity
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket sink = context.CreateSocket(SocketType.ROUTER), anonymous = context.CreateSocket(SocketType.REQ), identified = context.CreateSocket(SocketType.REQ))
                {
                    sink.Bind("inproc://example");

                    //  First allow 0MQ to set the identity
                    anonymous.Connect("inproc://example");
                    anonymous.Send("ROUTER uses a generated UUID", Encoding.Unicode);
                    ZHelpers.Dump(sink, Encoding.Unicode);

                    //  Then set the identity ourselves
                    identified.Identity = Encoding.Unicode.GetBytes("PEER2");
                    identified.Connect("inproc://example");
                    identified.Send("ROUTER socket uses REQ's socket identity", Encoding.Unicode);
                    ZHelpers.Dump(sink, Encoding.Unicode);
                }
            }
        }
    }
}
