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
		const int SyncPub_SubscribersExpected = 10;

		public static void SyncPub(IDictionary<string, string> dict, string[] args)
		{
			using (var context = ZContext.Create())
			using (var publisher = ZSocket.Create(context, ZSocketType.PUB))
			using (var syncservice = ZSocket.Create(context, ZSocketType.REP))
			{
				publisher.SendHighWatermark = 1100000;
				publisher.Bind("tcp://*:5561");

				syncservice.Bind("tcp://*:5562");

				Console.WriteLine("Waiting for subscribers");
				int subscribers = 0;
				while (subscribers < SyncPub_SubscribersExpected)
				{

					syncservice.ReceiveFrame();

					syncservice.SendFrame(new ZFrame());
					++subscribers;
				}

				Console.WriteLine("Broadcasting messages");
				int i = 0;
				for (; i < 1000000; ++i)
				{
					publisher.SendFrame(new ZFrame("Rhubarb"));
				}
				publisher.SendFrame(new ZFrame("END"));
			}
		}
	}
}