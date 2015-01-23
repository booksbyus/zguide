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
			using (var context = ZContext.Create())
			using (var receiver = ZSocket.Create(context, ZSocketType.PAIR))
			{
				receiver.Bind("inproc://step3");

				var thread = new Thread(() => MTRelay_step2(context));
				thread.Start();

				receiver.ReceiveFrame();

				Console.WriteLine("Test successful!");
			}
		}

		static void MTRelay_step2(ZContext context)
		{
			using (var receiver = ZSocket.Create(context, ZSocketType.PAIR))
			{
				receiver.Bind("inproc://step2");

				var thread = new Thread(() => MTRelay_step1(context));
				thread.Start();

				receiver.ReceiveFrame();
			}
			using (var xmitter = ZSocket.Create(context, ZSocketType.PAIR))
			{
				xmitter.Connect("inproc://step3");

				Console.WriteLine("Step 2 ready, signaling step 3");
				xmitter.Send(new ZFrame("READY"));
			}
		}

		static void MTRelay_step1(ZContext context) 
		{
			using (var xmitter = ZSocket.Create(context, ZSocketType.PAIR))
			{

				xmitter.Connect("inproc://step2");

				Console.WriteLine("Step 1 ready, signaling step 2");
				xmitter.Send(new ZFrame("READY"));
			}
		}

	}
}