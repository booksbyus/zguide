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
		public static void HWClient(IDictionary<string, string> dict, string[] args)
		{
			//
			// Hello World client
			//
			// Author: metadings (uli.riehm@metadea.de)
			//

			// Create
			using (var context = ZContext.Create())
			using (var requester = ZSocket.Create(context, ZSocketType.REQ))
			{
				// Connect
				requester.Connect("tcp://127.0.0.1:5555");

				for (int n = 0; n < 10; ++n)
				{
					string requestText = "Hello";
					Console.Write("Sending {0}...", requestText);

					// Send
					using (var request = new ZFrame(requestText)) 
					{
						requester.Send(request);
					}

					// Receive
					using (ZFrame reply = requester.ReceiveFrame()) 
					{
						Console.WriteLine(" Received: {0} {1}!", requestText, reply.ReadString());
					}
				}
			}
		}
	}
}