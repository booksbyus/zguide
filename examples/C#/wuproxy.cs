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
		public static void WUProxy(IDictionary<string, string> dict, string[] args)
		{
			//
			// Weather proxy device
			//
			// Authors: Pieter Hintjens, Uli Riehm
			//

			// frontend is where the weather server sits
			// backend is our public endpoint for subscribers
			using (var context = ZContext.Create())
			using (var frontend = ZSocket.Create(context, ZSocketType.XSUB))
			using (var backend = ZSocket.Create(context, ZSocketType.XPUB)) {

				frontend.Connect("tcp://192.168.1.10:5556");
				backend.Bind("tcp://10.1.1.0:8100");

				// Run the proxy until the user interrupts us
				ZContext.Proxy(frontend, backend);
			}
		}
	}
}