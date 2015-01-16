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
		static int RTReq_Workers = 10;

		public static void RTReq(IDictionary<string, string> dict, string[] args)
		{
			using (var context = ZContext.Create())
			using (var broker = ZSocket.Create(context, ZSocketType.ROUTER)) {

				broker.Bind("tcp://*:5671");

				for (int i = 0; i < RTReq_Workers; ++i) {
					int j = i;
					new Thread(() => RTReq_Worker(j)).Start();
				}

				var stopwatch = new Stopwatch();
				stopwatch.Start();

				int workers_fired = 0;
				while (true) {
					using (ZMessage identity = broker.ReceiveMessage()) {
						broker.SendFrameMore(identity[0]);
						broker.SendFrameMore(ZFrame.Create(string.Empty));

						if (stopwatch.Elapsed < TimeSpan.FromSeconds(5)) {
							broker.SendFrame(ZFrame.Create("Work harder!"));
						} else {
							broker.SendFrame(ZFrame.Create("Fired!"));
							if (++workers_fired == RTReq_Workers) {
								break;
							}
						}
					}
				}
			}
		}

		static void RTReq_Worker(int i) {
			using (var context = ZContext.Create()) 
			using (var worker = ZSocket.Create(context, ZSocketType.REQ)) {
				worker.Identity = Encoding.UTF8.GetBytes("PEER" + i);
				worker.Connect("tcp://127.0.0.1:5671");

				int total = 0;
				while (true) {
					worker.SendFrame(ZFrame.Create("Hi Boss"));

					bool finished;
					using (ZFrame frame = worker.ReceiveFrame()) {
						finished = (frame.ReadString() == "Fired!");
					}
					if (finished) break;

					total++;
					Thread.Sleep(1);
				}
				Console.WriteLine("Completed: PEER{0}, {1} tasks", i, total);
			}
		}
	}
}