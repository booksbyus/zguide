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
		public static void TaskVent(IDictionary<string, string> dict, string[] args)
		{
			using (var context = ZContext.Create())
			using (var sender = ZSocket.Create(context, ZSocketType.PUSH))
			using (var sink = ZSocket.Create(context, ZSocketType.PUSH))
			{
				sender.Bind("tcp://*:5557");
				sink.Connect("tcp://127.0.0.1:5558");

				Console.WriteLine("Press ENTER when the workers are ready...");
				Console.ReadKey(true);
				Console.WriteLine("Sending tasks to workers...");

				sink.Send(new byte[] { 0x00 }, 0, 1);

				int i = 0;
				long total_msec = 0;
				var rnd = new Random();
				for (; i < 100; ++i)
				{
					int workload = rnd.Next(100) + 1;
					total_msec += workload;
					byte[] action = BitConverter.GetBytes(workload);
					Console.WriteLine("{0}", workload);
					sender.Send(action, 0, action.Length);
				}

				Console.WriteLine("Total expected cost: {0} ms", total_msec);
			}
		}
	}
}