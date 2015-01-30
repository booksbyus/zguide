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
		public static void RRWorker(IDictionary<string, string> dict, string[] args)
		{
			//
			// Hello World worker
			// Connects REP socket to tcp://localhost:5560
			// Expects "Hello" from client, replies with "World"
			//
			// Authors: Uli Riehm
			//

			if (args == null || args.Length == 0)
			{
				args = new string[] { "World" };
			}
			string name = args[0];

			// Socket to talk to clients
			using (var context = ZContext.Create())
			using (var responder = ZSocket.Create(context, ZSocketType.REP))
			{
				responder.Connect("tcp://127.0.0.1:5560");

				while (true)
				{
					// Wait for next request from client
					using (ZFrame request = responder.ReceiveFrame())
					{
						Console.Write("{0} ", request.ReadString());
					}

					// Do some 'work'
					Thread.Sleep(1);

					// Send reply back to client
					Console.WriteLine("{0}... ", name);
					using (var reply = new ZFrame(name))
					{
						responder.Send(reply);
					}
				}
			}
		}
	}
}