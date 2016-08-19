using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading;

using ZeroMQ;

namespace Examples
{
	static partial class Program
	{
		public static void TaskSink(string[] args)
		{
			//
			// Task sink
			// Binds PULL socket to tcp://127.0.0.1:5558
			// Collects results from workers via that socket
			//
			// Author: metadings
			//

			// Prepare our context and socket
			using (var context = new ZContext())
			using (var sink = new ZSocket(context, ZSocketType.PULL))
			{
				sink.Bind("tcp://*:5558");

				// Wait for start of batch
				sink.ReceiveFrame();

				// Start our clock now
				var stopwatch = new Stopwatch();
				stopwatch.Start();

				// Process 100 confirmations
				for (int i = 0; i < 100; ++i)
				{
					sink.ReceiveFrame();

					if ((i / 10) * 10 == i)
						Console.Write(":");
					else
						Console.Write(".");
				}

				// Calculate and report duration of batch
				stopwatch.Stop();
				Console.WriteLine("Total elapsed time: {0} ms", stopwatch.ElapsedMilliseconds);
			}
		}
	}
}