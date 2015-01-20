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
		public static void SyncSub(IDictionary<string, string> dict, string[] args)
		{
			using (var context = ZContext.Create())
			using (var subscriber = ZSocket.Create(context, ZSocketType.SUB))
			using (var syncclient = ZSocket.Create(context, ZSocketType.REQ))
			{
				subscriber.Connect("tcp://127.0.0.1:5561");
				subscriber.SubscribeAll();

				syncclient.Connect("tcp://127.0.0.1:5562");

				syncclient.SendFrame(new ZFrame(string.Empty));
				syncclient.ReceiveFrame();

				int i = 0;
				while (true)
				{
					using (ZFrame frame = subscriber.ReceiveFrame())
					{
						if (frame.ReadString() == "END")
						{
							break;
						}
						++i;
					}
				}
				Console.WriteLine("Received {0} updates", i);
			}
		}
	}
}