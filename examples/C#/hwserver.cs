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
		public static void HWServer(IDictionary<string, string> dict, string[] args)
		{
			//
			// Hello World server
			//
			// Authors: Pieter Hintjens, Uli Riehm
			//

			// Create
			using (var context = ZContext.Create())
			using (var responder = ZSocket.Create(context, ZSocketType.REP))
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
						using (var reply = new ZFrame("World"))
						{
							responder.Send(reply);
						}
					}
				}
			}
		}
	}
}