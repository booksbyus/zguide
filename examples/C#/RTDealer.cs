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
		static int RTDealer_Workers = 10;

		public static void RTDealer(IDictionary<string, string> dict, string[] args)
		{
			using (var context = ZContext.Create())
			using (var broker = ZSocket.Create(context, ZSocketType.ROUTER))
			{
				broker.Bind("tcp://*:5671");

				for (int i = 0; i < RTDealer_Workers; ++i)
				{
					int j = i;
					new Thread(() => RTDealer_Worker(j)).Start();
				}

				var stopwatch = new Stopwatch();
				stopwatch.Start();

				int workers_fired = 0;
				while (true)
				{
					using (ZMessage identity = broker.ReceiveMessage())
					{
						broker.SendMore(identity[0]);
						broker.SendMore(new ZFrame());

						if (stopwatch.Elapsed < TimeSpan.FromSeconds(5))
						{
							broker.Send(new ZFrame("Work harder!"));
						}
						else
						{
							broker.Send(new ZFrame("Fired!"));
							if (++workers_fired == RTDealer_Workers)
							{
								break;
							}
						}
					}
				}
			}
		}

		static void RTDealer_Worker(int i) 
		{
			using (var context = ZContext.Create())
			using (var worker = ZSocket.Create(context, ZSocketType.DEALER))
			{
				worker.Identity = Encoding.UTF8.GetBytes("PEER" + i);
				worker.Connect("tcp://127.0.0.1:5671");

				int total = 0;
				while (true)
				{
					worker.SendMore(new ZFrame(worker.Identity, 0, worker.Identity.Length));
					worker.SendMore(new ZFrame());
					worker.Send(new ZFrame("Hi Boss"));	

					bool finished;
					using (ZMessage msg = worker.ReceiveMessage())
					{
						finished = (msg[2].ReadString() == "Fired!");
					}
					if (finished)
						break;

					total++;
					Thread.Sleep(1);
				}

				Console.WriteLine("Completed: PEER{0}, {1} tasks", i, total);
			}
		}
	}
}