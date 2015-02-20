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
		public static void MTServer(string[] args)
		{
			//
			// Multithreaded Hello World server
			//
			// Author: metadings
			//

			// Socket to talk to clients and
			// Socket to talk to workers
			using (var ctx = new ZContext())
			using (var clients = new ZSocket(ctx, ZSocketType.ROUTER))
			using (var workers = new ZSocket(ctx, ZSocketType.DEALER))
			{
				clients.Bind("tcp://*:5555");
				workers.Bind("inproc://workers");

				// Launch pool of worker threads
				for (int i = 0; i < 5; ++i)
				{
					new Thread(() => MTServer_Worker(ctx)).Start();
				}

				// Connect work threads to client threads via a queue proxy
				ZContext.Proxy(clients, workers);
			}
		}
		
		static void MTServer_Worker(ZContext ctx) 
		{
			// Socket to talk to dispatcher
			using (var server = new ZSocket(ctx, ZSocketType.REP))
			{
				server.Connect("inproc://workers");

				while (true)
				{
					using (ZFrame frame = server.ReceiveFrame())
					{
						Console.Write("Received: {0}", frame.ReadString());

						// Do some 'work'
						Thread.Sleep(1);

						// Send reply back to client
						string replyText = "World";
						Console.WriteLine(", Sending: {0}", replyText);
						server.Send(new ZFrame(replyText));
					}
				}
			}
		}
	}
}