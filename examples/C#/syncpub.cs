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
		const int SyncPub_SubscribersExpected = 3;	// We wait for 3 subscribers

		public static void SyncPub(IDictionary<string, string> dict, string[] args)
		{
			//
			// Synchronized publisher
			//
			// Author: metadings
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
				int subscribers = SyncPub_SubscribersExpected;
				do
				{
					Console.WriteLine("Waiting for {0} subscriber" + (subscribers > 1 ? "s" : string.Empty) + "...", subscribers);

					// - wait for synchronization request
					syncservice.ReceiveFrame();

					// - send synchronization reply
					syncservice.Send(new ZFrame());
				} 
				while (--subscribers > 0);

				// Now broadcast exactly 20 updates followed by END
				Console.WriteLine("Broadcasting messages:");
				for (int i = 0; i < 20; ++i)
				{
					Console.WriteLine("Sending {0}...", i);
					publisher.Send(new ZFrame(i));
				}
				publisher.Send(new ZFrame("END"));
			}
		}
	}
}