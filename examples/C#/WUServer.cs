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
		public static void WUServer(IDictionary<string, string> dict, string[] args)
		{
			using (var context = ZContext.Create())
			using (var publisher = ZSocket.Create(context, ZSocketType.PUB))
			{
				publisher.Bind("tcp://*:5556");
				// publisher.Bind("epgm://eth0,224.0.0.0:5556");

				var rnd = new Random();
				while (true)
				{
					// Get values that will fool the boss
					int zipcode = rnd.Next(99999);
					int temperature = rnd.Next(-55, +45);

					var update = string.Format("{0:D5} {1}", zipcode, temperature);
					using (var updateFrame = new ZFrame(update))
					{
						publisher.SendFrame(updateFrame);
					}
				}
			}
		}
	}
}