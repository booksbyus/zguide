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
		public static void HWServer(string[] args)
		{
			//
			// Hello World server
			//
			// Author: metadings
			//

			if (args == null || args.Length < 1)
			{
				Console.WriteLine();
				Console.WriteLine("Usage: ./{0} HWServer [Name]", AppDomain.CurrentDomain.FriendlyName);
				Console.WriteLine();
				Console.WriteLine("    Name   Your name. Default: World");
				Console.WriteLine();
				args = new string[] { "World" };
			}

			string name = args[0];

			// Create
			using (var context = new ZContext())
			using (var responder = new ZSocket(context, ZSocketType.REP))
			{
				// Bind
				responder.Bind("tcp://*:5555");

				while (true)
				{
					// Receive
					using (ZFrame request = responder.ReceiveFrame())
					{
						Console.WriteLine("Received {0}", request.ReadString());

						// Do some work
						Thread.Sleep(1);

						// Send
						responder.Send(new ZFrame(name));
					}
				}
			}
		}
	}
}