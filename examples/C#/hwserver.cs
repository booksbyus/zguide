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

			using (var context = ZContext.Create())
			using (var responder = ZSocket.Create(context, ZSocketType.REP))
			{
				responder.Bind("tcp://*:5555");

				while (true)
				{
					using (ZFrame request = responder.ReceiveFrame())
					{
						Console.WriteLine("Received {0}", request.ReadString());

						Thread.Sleep(1);

						using (ZFrame reply = new ZFrame("World"))
						{
							responder.Send(reply);
						}
					}
				}
			}
		}
	}
}