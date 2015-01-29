using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;

using ZeroMQ;

namespace ZeroMQ.Test
{
	static partial class Program
	{
		public static void WUClient(IDictionary<string, string> dict, string[] args)
		{
			//
			// Weather update client
			// Connects SUB socket to tcp://localhost:5556
			// Collects weather updates and finds avg temp in zipcode
			//
			// Authors: Pieter Hintjens, Uli Riehm
			//
			if (args == null || args.Length < 2)
			{
				Console.WriteLine();
				Console.WriteLine("Usage: ./{0} WUClient [ZipCode] [Endpoint]", AppDomain.CurrentDomain.FriendlyName);
				Console.WriteLine();
				Console.WriteLine("    ZipCode   The zip code to subscribe. Default is Nürtingen, 72622");
				Console.WriteLine("    Endpoint  Where the WUClient should connect to.");
				Console.WriteLine("              Default: tcp://127.0.0.1:5556");
				Console.WriteLine();
				if (args.Length < 1)
					args = new string[] { "72622", "tcp://127.0.0.1:5556" };
				else
					args = new string[] { args[0], "tcp://127.0.0.1:5556" };
			}

			// Socket to talk to server
			using (var context = ZContext.Create())
			using (var subscriber = ZSocket.Create(context, ZSocketType.SUB))
			{
				string connect_to = args[1];
				Console.WriteLine("I: connecting to {0}...", connect_to);
				subscriber.Connect(connect_to);

				foreach (IPAddress address in WUProxy_GetPublicIPs())
				{
					var epgmAddress = string.Format("epgm://{0};239.192.1.1:8100", address);
					Console.WriteLine("I: connecting to {0}...", epgmAddress);
					subscriber.Connect(epgmAddress);
				}

				// Subscribe to zipcode, default is NYC, 10001
				string zipCode = args[0];
				Console.WriteLine("I: Subscribing to zip code {0}...", zipCode);
				subscriber.Subscribe(zipCode);

				// Process 10 updates
				int i = 0;
				long total_temperature = 0;
				for (; i < 10; ++i)
				{
					using (var replyFrame = subscriber.ReceiveFrame())
					{
						string reply = replyFrame.ReadString();

						Console.WriteLine(reply);
						total_temperature += Convert.ToInt64(reply.Split(' ')[1]);
					}
				}
				Console.WriteLine("Average temperature for zipcode '{0}' was {1}°", zipCode, (total_temperature / i));
			}
		}
	}
}