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
		public static void TaskWork(IDictionary<string, string> dict, string[] args)
		{
			//
			// Task worker
			// Connects PULL socket to tcp://localhost:5557
			// Collects workloads from ventilator via that socket
			// Connects PUSH socket to tcp://localhost:5558
			// Sends results to sink via that socket
			//
			// Authors: Pieter Hintjens, Uli Riehm
			//

			// Socket to receive messages on and
			// Socket to send messages to
			using (var context = ZContext.Create())
			using (var receiver = ZSocket.Create(context, ZSocketType.PULL))
			using (var sink = ZSocket.Create(context, ZSocketType.PUSH))
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