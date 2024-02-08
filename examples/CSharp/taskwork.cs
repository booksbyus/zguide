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
		public static void TaskWork(string[] args)
		{
			//
			// Task worker
			// Connects PULL socket to tcp://127.0.0.1:5557
			// Collects workloads from ventilator via that socket
			// Connects PUSH socket to tcp://127.0.0.1:5558
			// Sends results to sink via that socket
			//
			// Author: metadings
			//

			// Socket to receive messages on and
			// Socket to send messages to
			using (var context = new ZContext())
			using (var receiver = new ZSocket(context, ZSocketType.PULL))
			using (var sink = new ZSocket(context, ZSocketType.PUSH))
			{
				receiver.Connect("tcp://127.0.0.1:5557");
				sink.Connect("tcp://127.0.0.1:5558");

				// Process tasks forever
				while (true)
				{
					var replyBytes = new byte[4];
					receiver.ReceiveBytes(replyBytes, 0, replyBytes.Length);
					int workload = BitConverter.ToInt32(replyBytes, 0);
					Console.WriteLine("{0}.", workload);	// Show progress

					Thread.Sleep(workload);	// Do the work

					sink.Send(new byte[0], 0, 0);	// Send results to sink
				}
			}
		}
	}
}