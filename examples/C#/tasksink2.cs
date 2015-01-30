using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading;

using ZeroMQ;

namespace ZeroMQ.Test
{
	static partial class Program
	{
		public static void TaskSink2(IDictionary<string, string> dict, string[] args)
		{
			//
			// Task sink - design 2
			// Adds pub-sub flow to send kill signal to workers
			//
			// Authors: Uli Riehm
			//

			// Socket to receive messages on and
			// Socket for worker control
			using (var context = ZContext.Create())
			using (var receiver = ZSocket.Create(context, ZSocketType.PULL))
			using (var controller = ZSocket.Create(context, ZSocketType.PUB))
			{
				receiver.Bind("tcp://*:5558");
				controller.Bind("tcp://*:5559");

				// Wait for start of batch
				receiver.ReceiveFrame();

				// Start our clock now
				var stopwatch = new Stopwatch();
				stopwatch.Start();

				// Process 100 confirmations
				for (int i = 0; i < 100; ++i)
				{
					receiver.ReceiveFrame();

					if ((i / 10) * 10 == i)
						Console.Write(":");
					else
						Console.Write(".");
				}

				stopwatch.Stop();
				Console.WriteLine("Total elapsed time: {0} ms", stopwatch.ElapsedMilliseconds);

				// Send kill signal to workers
				controller.Send(new ZFrame("KILL"));
			}
		}
	}
}