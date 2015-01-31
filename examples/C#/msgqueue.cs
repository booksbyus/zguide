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
		public static void MsgQueue(IDictionary<string, string> dict, string[] args)
		{
			//
			// Simple message queuing broker
			// Same as request-reply broker but using QUEUE device
			//
			// Author: metadings (uli.riehm@metadea.de)
			//

			// Socket facing clients and
			// Socket facing services
			using (var context = ZContext.Create())
			using (var frontend = ZSocket.Create(context, ZSocketType.ROUTER))
			using (var backend = ZSocket.Create(context, ZSocketType.DEALER))
			{
				frontend.Bind("tcp://*:5559");
				backend.Bind("tcp://*:5560");

				// Start the proxy
				ZContext.Proxy(frontend, backend);
			}
		}
	}
}