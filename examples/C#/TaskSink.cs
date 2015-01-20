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
		public static void TaskSink(IDictionary<string, string> dict, string[] args)
		{
			using (var context = ZContext.Create())
			using (var sink = ZSocket.Create(context, ZSocketType.PULL))
			{
				sink.Bind("tcp://*:5558");

				sink.ReceiveFrame();

				var stopwatch = new Stopwatch();
				stopwatch.Start();

				int i = 0;
				for (; i < 100; ++i)
				{
					sink.ReceiveFrame();

					if ((i / 10) * 10 == i)
						Console.Write(":");
					else
						Console.Write(".");
				}

				stopwatch.Stop();
				Console.WriteLine("Total elapsed time: {0} ms", stopwatch.ElapsedMilliseconds);
			}
		}
	}
}