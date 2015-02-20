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
		public static void MTRelay(string[] args)
		{
			//
			// Multithreaded relay
			//
			// Author: metadings
			//

			// Bind inproc socket before starting step2
			using (var ctx = new ZContext())
			using (var receiver = new ZSocket(ctx, ZSocketType.PAIR))
			{
				receiver.Bind("inproc://step3");

				new Thread(() => MTRelay_step2(ctx)).Start();

				// Wait for signal
				receiver.ReceiveFrame();

				Console.WriteLine("Test successful!");
			}
		}

		static void MTRelay_step2(ZContext ctx)
		{
			// Bind inproc socket before starting step1
			using (var receiver = new ZSocket(ctx, ZSocketType.PAIR))
			{
				receiver.Bind("inproc://step2");

				new Thread(() => MTRelay_step1(ctx)).Start();

				// Wait for signal and pass it on
				receiver.ReceiveFrame();
			}

			// Connect to step3 and tell it we're ready
			using (var xmitter = new ZSocket(ctx, ZSocketType.PAIR))
			{
				xmitter.Connect("inproc://step3");

				Console.WriteLine("Step 2 ready, signaling step 3");
				xmitter.Send(new ZFrame("READY"));
			}
		}

		static void MTRelay_step1(ZContext ctx) 
		{
			// Connect to step2 and tell it we're ready
			using (var xmitter = new ZSocket(ctx, ZSocketType.PAIR))
			{
				xmitter.Connect("inproc://step2");

				Console.WriteLine("Step 1 ready, signaling step 2");
				xmitter.Send(new ZFrame("READY"));
			}
		}

	}
}