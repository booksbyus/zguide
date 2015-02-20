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
		public static void HWClient(string[] args)
		{
			//
			// Hello World client
			//
			// Author: metadings
			//

			if (args == null || args.Length < 1)
			{
				Console.WriteLine();
				Console.WriteLine("Usage: ./{0} HWClient [Endpoint]", AppDomain.CurrentDomain.FriendlyName);
				Console.WriteLine();
				Console.WriteLine("    Endpoint  Where HWClient should connect to.");
				Console.WriteLine("              Default is tcp://127.0.0.1:5555");
				Console.WriteLine();
				args = new string[] { "tcp://127.0.0.1:5555" };
			}

			string endpoint = args[0];

			// Create
			using (var context = new ZContext())
			using (var requester = new ZSocket(context, ZSocketType.REQ))
			{
				// Connect
				requester.Connect(endpoint);

				for (int n = 0; n < 10; ++n)
				{
					string requestText = "Hello";
					Console.Write("Sending {0}...", requestText);

					// Send
					requester.Send(new ZFrame(requestText));

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