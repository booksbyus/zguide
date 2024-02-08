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
		public static void MSReader(string[] args)
		{
			//
			// Reading from multiple sockets
			// This version uses a simple recv loop
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

				// Process messages from both sockets
				// We prioritize traffic from the task ventilator
				ZError error;
				ZFrame frame;
				while (true)
				{
					while (true)
					{
						if (null != (frame = receiver.ReceiveFrame(ZSocketFlags.DontWait, out error)))
						{
							// Process task
						}
						else
						{
							if (error == ZError.ETERM)
								return;	// Interrupted
							if (error != ZError.EAGAIN)
								throw new ZException(error);

							break;
						}
					}

					while (true)
					{
						if (null != (frame = subscriber.ReceiveFrame(ZSocketFlags.DontWait, out error)))
						{
							// Process weather update
						}
						else
						{
							if (error == ZError.ETERM)
								return;	// Interrupted
							if (error != ZError.EAGAIN)
								throw new ZException(error);

							break;
						}
					}

					// No activity, so sleep for 1 msec
					Thread.Sleep(1);
				}
			}
		}
	}
}