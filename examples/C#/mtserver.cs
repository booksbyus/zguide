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
		public static void MTServer(IDictionary<string, string> dict, string[] args)
		{
			//
			// Multithreaded Hello World server
			//
			// Author: metadings
			//

			// Socket to talk to clients and
			// Socket to talk to workers
			using (var context = ZContext.Create())
			using (var clients = ZSocket.Create(context, ZSocketType.ROUTER))
			using (var workers = ZSocket.Create(context, ZSocketType.DEALER))
			{
				clients.Bind("tcp://*:5555");
				workers.Bind("inproc://workers");

				// Launch pool of worker threads
				for (int i = 0; i < 5; ++i)
				{
					var thread = new Thread(() => MTServer_Worker(context));
					thread.Start();
				}

				// Connect work threads to client threads via a queue proxy
				ZContext.Proxy(clients, workers);
			}
		}
		
		static void MTServer_Worker(ZContext context) 
		{
			// Socket to talk to dispatcher
			using (var server = ZSocket.Create(context, ZSocketType.REP))
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