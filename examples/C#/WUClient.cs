using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

using ZeroMQ;
using ZeroMQ.lib;

namespace ZeroMQ.Test
{
	static partial class Program
	{
		public static void WUClient(IDictionary<string, string> dict, string[] args)
		{
			using (var context = ZContext.Create())
			using (var subscriber = ZSocket.Create(context, ZSocketType.SUB)) {

				subscriber.Connect("tcp://127.0.0.1:5556");

				string zipCode = "72622";
				subscriber.Subscribe(Encoding.UTF8.GetBytes(zipCode));

				int i = 0;
				long total_temperature = 0;
				for (; i < 100; ++i) {

					using (var replyFrame = subscriber.ReceiveFrame()) {
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