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
			//
			// Synchronized subscriber
			//
			// Authors: Pieter Hintjens, Uli Riehm
			//

			using (var context = ZContext.Create())
			using (var subscriber = ZSocket.Create(context, ZSocketType.SUB))
			using (var syncclient = ZSocket.Create(context, ZSocketType.REQ))
			{
				// First, connect our subscriber socket
				subscriber.Connect("tcp://127.0.0.1:5561");
				subscriber.SubscribeAll();

				// 0MQ is so fast, we need to wait a while…
				Thread.Sleep(1);

				// Second, synchronize with publisher
				syncclient.Connect("tcp://127.0.0.1:5562");

				// - send a synchronization request
				syncclient.Send(new ZFrame(string.Empty));

				// - wait for synchronization reply
				syncclient.ReceiveFrame();

				// Third, get our updates and report how many we got
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