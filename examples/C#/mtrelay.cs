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
		public static void MTRelay(IDictionary<string, string> dict, string[] args)
		{
			//
			// Multithreaded relay
			//
			// Author: metadings
			//

			// Bind inproc socket before starting step2
			using (var context = ZContext.Create())
			using (var receiver = ZSocket.Create(context, ZSocketType.PAIR))
			{
				receiver.Bind("inproc://step3");

				var thread = new Thread(() => MTRelay_step2(context));
				thread.Start();

				// Wait for signal
				receiver.ReceiveFrame();

				Console.WriteLine("Test successful!");
			}
		}

		static void MTRelay_step2(ZContext context)
		{
			// Bind inproc socket before starting step1
			using (var receiver = ZSocket.Create(context, ZSocketType.PAIR))
			{
				receiver.Bind("inproc://step2");

				var thread = new Thread(() => MTRelay_step1(context));
				thread.Start();

				// Wait for signal and pass it on
				receiver.ReceiveFrame();
			}

			// Connect to step3 and tell it we're ready
			using (var xmitter = ZSocket.Create(context, ZSocketType.PAIR))
			{
				xmitter.Connect("inproc://step3");

				Console.WriteLine("Step 2 ready, signaling step 3");
				xmitter.Send(new ZFrame("READY"));
			}
		}

		static void MTRelay_step1(ZContext context) 
		{
			// Connect to step2 and tell it we're ready
			using (var xmitter = ZSocket.Create(context, ZSocketType.PAIR))
			{
				xmitter.Connect("inproc://step2");

				Console.WriteLine("Step 1 ready, signaling step 2");
				xmitter.Send(new ZFrame("READY"));
			}
		}

	}
}