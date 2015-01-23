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

				var poll = ZPollItem.CreateReceiver();

				ZError error;
				ZMessage message;
				while (true)
				{
					if (frontend.PollIn(poll, out message, out error, TimeSpan.FromMilliseconds(64)))
					{
						backend.Send(message, out error);
					}
					if (backend.PollIn(poll, out message, out error, TimeSpan.FromMilliseconds(64)))
					{
						frontend.Send(message, out error);
					}
				}
			}
		}
	}
}