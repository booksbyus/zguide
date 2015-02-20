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
		public static void PathoPub(string[] args)
		{
			//
			// Pathological publisher
			// Sends out 1,000 topics and then one random update per second
			//
			// Author: metadings
			//

			if (args == null || args.Length < 1)
			{
				Console.WriteLine();
				Console.WriteLine("Usage: ./{0} PathoPub [Endpoint]", AppDomain.CurrentDomain.FriendlyName);
				Console.WriteLine();
				Console.WriteLine("    Endpoint  Where PathoPub should connect to.");
				Console.WriteLine("              Default is null, Binding on tcp://*:5556");
				Console.WriteLine();
				args = new string[] { null };
			}

			using (var context = new ZContext())
			using (var publisher = new ZSocket(context, ZSocketType.PUB))
			{
				if (args[0] != null)
				{
					publisher.Connect(args[0]);
				}
				else
				{
					publisher.Bind("tcp://*:5556");
				}

				// Ensure subscriber connection has time to complete
				Thread.Sleep(100);

				// Send out all 1,000 topic messages
				for (int topic = 0; topic < 1000; ++topic)
				{
					publisher.SendMore(new ZFrame(string.Format("{0:D3}", topic)));
					publisher.Send(new ZFrame("Save Roger"));
				}

				// Send one random update per second
				var rnd = new Random();
				while (true)
				{
					Thread.Sleep(10);
					publisher.SendMore(new ZFrame(string.Format("{0:D3}", rnd.Next(1000))));
					publisher.Send(new ZFrame("Off with his head!"));
				}
			}
		}
	}
}