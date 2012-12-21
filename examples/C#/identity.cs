//
//  Demonstrate identities as used by the request-reply pattern.
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System.Text;
using ZeroMQ;
using zguide;

namespace ZMQGuide
{
    internal class Program3
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

                    //  Then set the identity ourself
                    identified.StringToIdentity("Hello", Encoding.Unicode);
                    identified.Connect("inproc://example");
                    identified.Send("ROUTER ZmqSocket uses REQ's ZmqSocket identity", Encoding.Unicode);
                    ZHelpers.Dump(sink, Encoding.Unicode);
                }
            }
        }
    }
}
