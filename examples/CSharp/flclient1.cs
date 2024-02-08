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
		static readonly TimeSpan FLClient1_REQUEST_TIMEOUT = TimeSpan.FromMilliseconds(2500);

		static int FLClient1_MAX_RETRIES = 3;	// Before we abandon

		public static void FLClient1(string[] args)
		{
			//
			// Freelance client - Model 1
			// Uses REQ socket to query one or more services
			//
			// Author: metadings
			//

			if (args == null || args.Length < 1)
			{
				Console.WriteLine();
				Console.WriteLine("Usage: ./{0} FLClient1 [Endpoint]", AppDomain.CurrentDomain.FriendlyName);
				Console.WriteLine();
				Console.WriteLine("    Endpoint  Where FLClient1 should connect to.");
				Console.WriteLine("              Default is tcp://127.0.0.1:7780");
				Console.WriteLine();
				args = new string[] { "tcp://127.0.0.1:7780" };
			}

			// The client uses a Lazy Pirate strategy if it only has one server to talk
			// to. If it has two or more servers to talk to, it will try each server just
			// once:

			using (var context = new ZContext())
			using (var request = new ZFrame("Hello World"))
			{
				ZFrame reply = null;

				if (args.Length == 1)
				{
					// For one endpoint, we retry N times
					string endpoint = args[0];

					for (int retries = 0; retries < FLClient1_MAX_RETRIES; ++retries)
					{
						if (null != (reply = FLClient1_TryRequest(context, endpoint, request)))
						{
							break;	// Successful
						}
						Console.WriteLine("W: no response from {0}, retrying...", endpoint);
					}
				}
				else
				{
					// For multiple endpoints, try each at most once

					for (int endpoint_nbr = 0; endpoint_nbr < args.Length; ++endpoint_nbr)
					{
						string endpoint = args[endpoint_nbr];

						if (null != (reply = FLClient1_TryRequest(context, endpoint, request)))
						{
							break;	// Successful
						}
						Console.WriteLine("W: no response from {0}, retrying...", endpoint);
					}
				}

				if (reply != null)
				{
					Console.WriteLine("Service is running OK");
				}
			}
		}

		static ZFrame FLClient1_TryRequest(ZContext context, string endpoint, ZFrame request)
		{
			Console.WriteLine("I: trying echo service at {0}...", endpoint);

			using (var client = new ZSocket(context, ZSocketType.REQ))
			{
				client.Connect(endpoint);

				// Send request, wait safely for reply
				using (var message = ZFrame.CopyFrom(request))
				{
					client.Send(message);
				}

				var poll = ZPollItem.CreateReceiver();
				ZError error;
				ZMessage incoming;

				if (client.PollIn(poll, out incoming, out error, FLClient1_REQUEST_TIMEOUT))
				{
					return incoming[0];
				}
			}
			return null;
		}
	}
}