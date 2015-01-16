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
			using (var subscriber = ZSocket.Create(context, ZSocketType.SUB)) {

				receiver.Connect("tcp://127.0.0.1:5557");

				subscriber.Connect("tcp://127.0.0.1:5556");
				subscriber.SetOption(ZSocketOption.SUBSCRIBE, "72622 ");

				var pollers = new ZPollItem[] {
					ZPollItem.CreateReceiver(receiver),
					ZPollItem.CreateReceiver(subscriber)
				};

				ZError error;
				ZMessage msg;
				while (true) {

					if (pollers[0].PollIn(out msg, out error, TimeSpan.FromMilliseconds(64))) {
						// Process task
					}
					if (pollers[1].PollIn(out msg, out error, TimeSpan.FromMilliseconds(64))) {
						// Process weather update
					}
				}
			}
		}
	}
}