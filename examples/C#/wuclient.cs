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
			if (args == null || args.Length == 0)
			{
				args = new string[] { "10001", "tcp://127.0.0.1:5556" };
			}

			// Subscribe to zipcode, default is NYC, 10001
			string zipCode = args[0];

			string connect_to = args[0];

			// Socket to talk to server
			using (var context = ZContext.Create())
			using (var subscriber = ZSocket.Create(context, ZSocketType.SUB))
			{
				Console.WriteLine("I: connecting to localhost...");
				subscriber.Connect(connect_to);
				foreach (IPAddress address in WUProxy_GetPublicIPs())
				{
					Console.WriteLine("I: connecting to {0}...", address);
					subscriber.Connect(string.Format("epgm://{0};239.192.1.1:8100", address));
				}

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
				Console.WriteLine("Average temperature for zipcode '{0}' was {1}°.", zipCode, (total_temperature / i));
			}
		}
	}
}