using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ZeroMQ;

namespace zguide
{
    public class ZHelpers
    {
        public static void Dump(ZmqSocket socket, Encoding encoding)
        {
            if (socket == null)
            {
                throw new ArgumentNullException("socket");
            }

            Console.WriteLine(new String('-', 38));
            ZmqMessage message = socket.ReceiveMessage();
            foreach (var frame in message)
            {
                Console.Write("[{0:d3}] ", frame.BufferSize);

                if (frame.BufferSize == 5 && frame.Buffer[0] == 0)
                    Console.WriteLine("{0}", BitConverter.ToString(frame.Buffer).Replace("-", string.Empty));
                else
                    Console.WriteLine("{0}", encoding.GetString(frame.Buffer));
            }
        }

        public static string SetID(ZmqSocket client, Encoding unicode)
        {
            var str = client.GetHashCode().ToString();
            client.Identity = unicode.GetBytes(str);
            return str;
        }

        public static bool Version()
        {
            throw new NotImplementedException();
        }
    }
}
