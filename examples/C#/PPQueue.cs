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
		public static void PPQueue(IDictionary<string, string> dict, string[] args)
		{

			throw new NotImplementedException();

			/* Simple Pirate broker
			// This is identical to load-balancing pattern, with no reliability
			// mechanisms. It depends on the client for recovery. Runs forever.

			using (var context = ZContext.Create())
			using (var frontend = ZSocket.Create(context, ZSocketType.ROUTER))
			using (var backend = ZSocket.Create(context, ZSocketType.ROUTER))
			{
				frontend.Bind("tcp://*:5555");
				backend.Bind("tcp://*:5556");


			} /**/
		}
	}
}