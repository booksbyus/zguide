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
		public static void MSPoller(string[] args)
		{
			//
			// Reading from multiple sockets
			// This version uses zmq_poll()
			//
			// Author: metadings
			//

			using (var context = new ZContext())
			using (var receiver = new ZSocket(context, ZSocketType.PULL))
			using (var subscriber = new ZSocket(context, ZSocketType.SUB))
			{
				// Connect to task ventilator
				receiver.Connect("tcp://127.0.0.1:5557");

				// Connect to weather server
				subscriber.Connect("tcp://127.0.0.1:5556");
				subscriber.SetOption(ZSocketOption.SUBSCRIBE, "10001 ");

				var poll = ZPollItem.CreateReceiver();

				// Process messages from both sockets
				ZError error;
				ZMessage msg;
				while (true)
				{
					if (receiver.PollIn(poll, out msg, out error, TimeSpan.FromMilliseconds(64)))
					{
						// Process task
					}
					else
					{
						if (error == ZError.ETERM)
							return;	// Interrupted
						if (error != ZError.EAGAIN)
							throw new ZException(error);
					}

					if (subscriber.PollIn(poll, out msg, out error, TimeSpan.FromMilliseconds(64)))
					{
						// Process weather update
					}
					else
					{
						if (error == ZError.ETERM)
							return;	// Interrupted
						if (error != ZError.EAGAIN)
							throw new ZException(error);
					}
				}
			}
		}
	}
}