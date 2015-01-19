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
		public static void PSEnvPub(IDictionary<string, string> dict, string[] args)
		{
			using (var context = ZContext.Create())
			using (var publisher = ZSocket.Create(context, ZSocketType.PUB))
			{
				publisher.Bind("tcp://*:5563");

				while (true)
				{
					using (var message = new ZMessage())
					{
						message.Add(ZFrame.From("A"));
						message.Add(ZFrame.From("We don't want to see this"));
						publisher.SendMessage(message);
					}
					using (var message = new ZMessage())
					{
						message.Add(ZFrame.From("B"));
						message.Add(ZFrame.From("We would like to see this"));
						publisher.SendMessage(message);
					}
					Thread.Sleep(1);
				}
			}
		}
	}
}