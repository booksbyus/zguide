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
		public static void MsgQueue(string[] args)
		{
			//
			// Simple message queuing broker
			// Same as request-reply broker but using QUEUE device
			//
			// Author: metadings
			//

			// Socket facing clients and
			// Socket facing services
			using (var context = new ZContext())
			using (var frontend = new ZSocket(context, ZSocketType.ROUTER))
			using (var backend = new ZSocket(context, ZSocketType.DEALER))
			{
				frontend.Bind("tcp://*:5559");
				backend.Bind("tcp://*:5560");

				// Start the proxy
				ZContext.Proxy(frontend, backend);
			}
		}
	}
}