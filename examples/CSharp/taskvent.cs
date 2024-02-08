using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

using ZeroMQ;

namespace Examples
{
	static partial class Program
	{
		public static void TaskVent(string[] args)
		{
			//
			// Task ventilator
			// Binds PUSH socket to tcp://127.0.0.1:5557
			// Sends batch of tasks to workers via that socket
			//
			// Author: metadings
			//

			// Socket to send messages on and
			// Socket to send start of batch message on
			using (var context = new ZContext())
			using (var sender = new ZSocket(context, ZSocketType.PUSH))
			using (var sink = new ZSocket(context, ZSocketType.PUSH))
			{
				sender.Bind("tcp://*:5557");
				sink.Connect("tcp://127.0.0.1:5558");

				Console.WriteLine("Press ENTER when the workers are ready...");
				Console.ReadKey(true);
				Console.WriteLine("Sending tasks to workers...");

				// The first message is "0" and signals start of batch
				sink.Send(new byte[] { 0x00 }, 0, 1);

				// Initialize random number generator
				var rnd = new Random();

				// Send 100 tasks
				int i = 0;
				long total_msec = 0;	// Total expected cost in msecs
				for (; i < 100; ++i)
				{
					// Random workload from 1 to 100msecs
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