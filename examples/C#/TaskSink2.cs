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
			using (var context = ZContext.Create())
			using (var receiver = ZSocket.Create(context, ZSocketType.PULL))
			using (var controller = ZSocket.Create(context, ZSocketType.PUB))
			{
				receiver.Bind("tcp://*:5558");
				controller.Bind("tcp://*:5559");

				receiver.ReceiveFrame();

				var stopwatch = new Stopwatch();
				stopwatch.Start();

				int i = 0;
				for (; i < 100; ++i)
				{
					receiver.ReceiveFrame();

					if ((i / 10) * 10 == i)
						Console.Write(":");
					else
						Console.Write(".");
				}

				stopwatch.Stop();
				Console.WriteLine("Total elapsed time: {0} ms", stopwatch.ElapsedMilliseconds);

				controller.SendFrame(ZFrame.Create("KILL"));
			}
		}
	}
}