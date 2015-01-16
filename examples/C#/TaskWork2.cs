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
		public static void TaskWork2(IDictionary<string, string> dict, string[] args)
		{
			using (var context = ZContext.Create())
			using (var receiver = ZSocket.Create(context, ZSocketType.PULL))
			using (var sender = ZSocket.Create(context, ZSocketType.PUSH))
			using (var controller = ZSocket.Create(context, ZSocketType.SUB)) {

				receiver.Connect("tcp://127.0.0.1:5557");

				sender.Connect("tcp://127.0.0.1:5558");

				controller.Connect("tcp://127.0.0.1:5559");
				controller.SubscribeAll();

				var pollers = new ZPollItem[] {
					ZPollItem.CreateReceiver(receiver),
					ZPollItem.CreateReceiver(controller)
				};

				ZError error;
				ZMessage message;
				while (true) {

					if (pollers[0].PollIn(out message, out error, TimeSpan.FromMilliseconds(64))) {

						int workload = message.ReadInt32();
						Console.WriteLine("{0}.", workload);
						Thread.Sleep(workload);

						sender.Send(new byte[0], 0, 0);
					}
					if (pollers[1].PollIn(out message, out error, TimeSpan.FromMilliseconds(64))) {
						break;
					}
				}
			}
		}
	}
}