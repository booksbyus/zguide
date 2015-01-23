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
						message.Add(new ZFrame("A"));
						message.Add(new ZFrame("We don't want to see this"));
						publisher.Send(message);
					}
					using (var message = new ZMessage())
					{
						message.Add(new ZFrame("B"));
						message.Add(new ZFrame("We would like to see this"));
						publisher.Send(message);
					}
					Thread.Sleep(1);
				}
			}
		}
	}
}