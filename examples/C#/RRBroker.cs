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
		public static void RRBroker(IDictionary<string, string> dict, string[] args)
		{
			using (var context = ZContext.Create())
			using (var frontend = ZSocket.Create(context, ZSocketType.ROUTER))
			using (var backend = ZSocket.Create(context, ZSocketType.DEALER))
			{
				frontend.Bind("tcp://*:5559");
				backend.Bind("tcp://*:5560");

				var pollers = new ZPollItem[]
				{
					ZPollItem.CreateReceiver(frontend),
					ZPollItem.CreateReceiver(backend)
				};

				ZError error;
				ZMessage message;
				while (true)
				{
					if (pollers[0].PollIn(out message, out error, TimeSpan.FromMilliseconds(64)))
					{
						backend.SendMessage(message, out error);
					}
					if (pollers[1].PollIn(out message, out error, TimeSpan.FromMilliseconds(64)))
					{
						frontend.SendMessage(message, out error);
					}
				}
			}
		}
	}
}