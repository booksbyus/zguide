using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading;

using ZeroMQ;

namespace ZeroMQ.Test
{
	// Lets us build this source without creating a library
	using FLCli;

	static partial class Program
	{

		public static void FLClient3(IDictionary<string, string> dict, string[] args)
		{
			//
			// Freelance client - Model 3
			// Uses flcliapi class to encapsulate Freelance pattern
			//
			// Authors: Pieter Hintjens, Uli Riehm
			//

			// Create new freelance client object
			using (var client = new FLCliApi())
			{
				// Connect to several endpoints
				client.Connect("tcp://127.0.0.1:5555");
				client.Connect("tcp://127.0.0.1:5556");
				client.Connect("tcp://127.0.0.1:5557");

				// Send a bunch of name resolution 'requests', measure time
				var stopwatch = new Stopwatch();
				stopwatch.Start();

				int requests = 0;
				while (requests++ < 1000)
				{
					using (var request = new ZMessage())
					{
						request.Add(new ZFrame("random name"));

						using (ZMessage reply = client.Request(request))
						{

						}
					}
				}

				stopwatch.Stop();
				Console.WriteLine("Average round trip cost: {0} ms", stopwatch.ElapsedMilliseconds / requests);
			}
		}
	}
}