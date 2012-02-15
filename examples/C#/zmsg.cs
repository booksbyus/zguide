//
//  Multipart message class for example applications.

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System.Collections.Generic;
using System.Text;
using ZMQ;

namespace ZMQ
{
    public class ZMessage
    {
        readonly Encoding encoding = Encoding.Unicode;
        readonly List<byte[]> frames = new List<byte[]>();

        public ZMessage()
        {
        }

        public ZMessage(Socket socket)
        {
            Recv(socket);
        }

        public ZMessage(string body, Encoding encoding)
        {
            frames.Add(encoding.GetBytes(body));
            this.encoding = encoding;
        }

        public ZMessage(string body)
        {
            frames.Add(encoding.GetBytes(body));
        }

        public ZMessage(byte[] body)
        {
            frames.Add(body);
        }

        public void Recv(Socket socket)
        {
            Queue<byte[]> receivedFrames = socket.RecvAll();

            while (receivedFrames.Count > 0)
            {
                frames.Insert(0, (receivedFrames.Dequeue()));
            }
        }

        public void Send(Socket socket)
        {
            for (int index = frames.Count - 1; index > 0; index--)
            {
                socket.SendMore(frames[index]);
            }

            socket.Send(frames[0]);
        }

        public string BodyToString()
        {
            return encoding.GetString(Body);
        }

        public void StringToBody(string body)
        {
            Body = encoding.GetBytes(body);
        }

        public void Append(byte[] data)
        {
            frames.Insert(0, data);
        }

        public byte[] Pop()
        {
            byte[] data = frames[frames.Count - 1];
            frames.RemoveAt(frames.Count - 1);
            return data;
        }

        public void Push(byte[] data)
        {
            frames.Add(data);
        }

        public void Wrap(byte[] address, byte[] delim)
        {
            if (delim != null)
            {
                frames.Add(delim);
            }
            frames.Add(address);
        }

        public byte[] Unwrap()
        {
            byte[] addr = Pop();
            if (Address.Length == 0)
            {
                Pop();
            }
            return addr;
        }

        public int FrameCount
        {
            get { return frames.Count; }
        }

        public byte[] Address
        {
            get { return frames[frames.Count - 1]; }
            set { frames.Add(value); }
        }

        public byte[] Body
        {
            get { return frames[0]; }
            set { frames[0] = value; }
        }
    }
}

