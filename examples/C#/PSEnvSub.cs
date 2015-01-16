using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

using ZeroMQ;

namespace ZeroMQ.Test
{
	static partial class Program
	{
		public static void PSEnvSub(IDictionary<string, string> dict, string[] args)
		{
			using (var context = ZContext.Create())
			using (var subscriber = ZSocket.Create(context, ZSocketType.SUB))
			{
				subscriber.Connect("tcp://127.0.0.1:5563");
				subscriber.Subscribe(Encoding.UTF8.GetBytes("B"));

				while (true)
				{
					using (ZMessage message = subscriber.ReceiveMessage())
					{
						string address = message[0].ReadString();
						string contents = message[1].ReadString();
						Console.WriteLine("[{0}] {1}", address, contents);
					}
					Thread.Sleep(1);
				}
			}
		}
	}
}