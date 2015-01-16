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
			using (var context = ZContext.Create())
			using (var clients = ZSocket.Create(context, ZSocketType.ROUTER))
			using (var workers = ZSocket.Create(context, ZSocketType.DEALER))
			{
				clients.Bind("tcp://*:5555");
				workers.Bind("inproc://workers");

				int i = 0;
				for (; i < 5; ++i)
				{
					var thread = new Thread(() => MTServer_Worker(context));
					thread.Start();
				}

				ZContext.Proxy(clients, workers);
			}
		}
		
		static void MTServer_Worker(ZContext context) 
		{
			using (var server = ZSocket.Create(context, ZSocketType.REP))
			{
				server.Connect("inproc://workers");

				while (true)
				{
					using (ZFrame frame = server.ReceiveFrame())
					{
						Console.Write("Received: {0}", frame.ReadString());

						Thread.Sleep(1);

						string replyText = "World";
						Console.WriteLine(", Sending: {0}", replyText);
						server.SendFrame(ZFrame.Create(replyText));
					}
				}
			}
		}
	}
}