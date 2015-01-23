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
			using (var context = ZContext.Create())
			using (var receiver = ZSocket.Create(context, ZSocketType.PULL))
			using (var sink = ZSocket.Create(context, ZSocketType.PUSH))
			{
				receiver.Connect("tcp://127.0.0.1:5557");
				sink.Connect("tcp://127.0.0.1:5558");

				while (true)
				{
					var replyBytes = new byte[4];
					receiver.ReceiveBytes(replyBytes, 0, replyBytes.Length);
					int workload = BitConverter.ToInt32(replyBytes, 0);
					Console.WriteLine("{0}.", workload);
					Thread.Sleep(workload);
					sink.Send(new byte[0], 0, 0);
				}
			}
		}
	}
}