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
		const int SyncPub_SubscribersExpected = 10;	// We wait for 10 subscribers

		public static void SyncPub(IDictionary<string, string> dict, string[] args)
		{
			//
			// Synchronized publisher
			//
			// Authors: Uli Riehm
			//

			// Socket to talk to clients and
			// Socket to receive signals
			using (var context = ZContext.Create())
			using (var publisher = ZSocket.Create(context, ZSocketType.PUB))
			using (var syncservice = ZSocket.Create(context, ZSocketType.REP))
			{
				publisher.SendHighWatermark = 1100000;
				publisher.Bind("tcp://*:5561");

				syncservice.Bind("tcp://*:5562");

				// Get synchronization from subscribers
				Console.WriteLine("Waiting for subscribers");
				int subscribers = 0;
				while (subscribers < SyncPub_SubscribersExpected)
				{
					// - wait for synchronization request
					syncservice.ReceiveFrame();

					// - send synchronization reply
					syncservice.Send(new ZFrame());
					++subscribers;
				}

				// Now broadcast exactly 1M updates followed by END
				Console.WriteLine("Broadcasting messages");
				for (int i = 0; i < 1000000; ++i)
				{
					publisher.Send(new ZFrame("Rhubarb"));
				}
				publisher.Send(new ZFrame("END"));
			}
		}
	}
}