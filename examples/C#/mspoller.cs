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
		public static void MSPoller(IDictionary<string, string> dict, string[] args)
		{
			using (var context = ZContext.Create())
			using (var receiver = ZSocket.Create(context, ZSocketType.PULL))
			using (var subscriber = ZSocket.Create(context, ZSocketType.SUB))
			{
				receiver.Connect("tcp://127.0.0.1:5557");

				subscriber.Connect("tcp://127.0.0.1:5556");
				subscriber.SetOption(ZSocketOption.SUBSCRIBE, "72622 ");

				var poll = ZPollItem.CreateReceiver();

				ZError error;
				ZMessage msg;
				while (true)
				{
					if (receiver.PollIn(poll, out msg, out error, TimeSpan.FromMilliseconds(64)))
					{
						// Process task
					}
					if (subscriber.PollIn(poll, out msg, out error, TimeSpan.FromMilliseconds(64)))
					{
						// Process weather update
					}
				}
			}
		}
	}
}