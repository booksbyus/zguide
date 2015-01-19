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
		public static void RRServer(IDictionary<string, string> dict, string[] args)
		{
			using (var context = ZContext.Create())
			using (var responder = ZSocket.Create(context, ZSocketType.REP))
			{
				responder.Connect("tcp://127.0.0.1:5560");

				while (true)
				{
					using (ZFrame request = responder.ReceiveFrame())
					{
						Console.Write("Received {0}, ", request.ReadString());
					}

					Thread.Sleep(1);

					string replyText = "World";
					Console.WriteLine("Sending {0}... ", replyText);
					using (var reply = ZFrame.From(replyText))
					{
						responder.SendFrame(reply);
					}
				}
			}
		}
	}
}