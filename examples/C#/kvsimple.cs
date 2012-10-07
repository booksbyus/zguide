
//
//  Key-value message class in C#
//

//  Author:     Yan Cui
//  Email:      theburningmonk@gmail.com

using System;
using System.Collections.Generic;
using System.Text;

using ZMQ;

namespace ZMQGuide
{
    public class KvMsg
    {
        public KvMsg(long sequence, string key = "", string body = "")
        {
            Sequence = sequence;
            Key = key;
            Body = body;
        }

        public long Sequence { get; set; }

        public string Key { get; set; }

        public string Body { get; set; }

        public static KvMsg Recv(Socket subscriber)
        {
            var key = subscriber.Recv(Encoding.Unicode);
            var sequence = BitConverter.ToInt64(subscriber.Recv(), 0);
            var body = subscriber.Recv(Encoding.Unicode);

            return new KvMsg(sequence, key, body);
        }

        public void Send(Socket publisher) 
        {
            publisher.SendMore(Key, Encoding.Unicode);
            publisher.SendMore(BitConverter.GetBytes(Sequence));
            publisher.Send(Body, Encoding.Unicode);
        }

        public void Store(IDictionary<string, KvMsg> dict)
        {
            if (!string.IsNullOrWhiteSpace(Key) && !string.IsNullOrWhiteSpace(Body))
            {
                dict[Key] = this;
            }
        }

        public override string ToString()
        {
            if (string.IsNullOrWhiteSpace(Body))
            {
                return "NULL";
            }

            return string.Format(
                "[seq:{0}][key:{1}][size:{2}] {3}", 
                Sequence, 
                Key, 
                Body.Length, 
                Body);
        }
    }
}
