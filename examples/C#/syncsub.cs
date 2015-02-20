using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

using ZeroMQ;

namespace Examples
{
	static partial class Program
	{
		public static void SyncSub(string[] args)
		{
			//
			// Synchronized subscriber
			//
			// Author: metadings
			//

			using (var context = new ZContext())
			using (var subscriber = new ZSocket(context, ZSocketType.SUB))
			using (var syncclient = new ZSocket(context, ZSocketType.REQ))
			{
				// First, connect our subscriber socket
				subscriber.Connect("tcp://127.0.0.1:5561");
				subscriber.SubscribeAll();

				// 0MQ is so fast, we need to wait a while…
				Thread.Sleep(1);

				// Second, synchronize with publisher
				syncclient.Connect("tcp://127.0.0.1:5562");

				// - send a synchronization request
				syncclient.Send(new ZFrame());

				// - wait for synchronization reply
				syncclient.ReceiveFrame();

				// Third, get our updates and report how many we got
				int i = 0;
				while (true)
				{
					using (ZFrame frame = subscriber.ReceiveFrame())
					{
						string text = frame.ReadString();
						if (text == "END")
						{
							break;
						}

						frame.Position = 0;
						Console.WriteLine("Receiving {0}...", frame.ReadInt32());

						++i;
					}
				}
				Console.WriteLine("Received {0} updates.", i);
			}
		}
	}
}