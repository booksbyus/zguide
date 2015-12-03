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
		public static void Espresso0(string[] args)
		{
			//
			// Espresso Pattern, like orig C implementation
			// This shows how to capture data using a pub-sub proxy
			//
			// Author: chubbson
			//

			using (var context = new ZContext())
			using (var subscriber = new ZSocket(context, ZSocketType.XSUB))
			using (var publisher = new ZSocket(context, ZSocketType.XPUB))
			using (var listener = new ZSocket(context, ZSocketType.PAIR))
			{
				new Thread(() => Espresso0_Publisher(context)).Start();
				new Thread(() => Espresso0_Subscriber(context)).Start();
				new Thread(() => Espresso0_Listener(context)).Start();

				subscriber.Connect("tcp://127.0.0.1:6000");
				publisher.Bind("tcp://*:6001");
				listener.Bind("inproc://listener");

				ZError error;
				if (!ZContext.Proxy(subscriber, publisher, listener, out error))
				{
					if (error == ZError.ETERM)
						return;	// Interrupted
					throw new ZException(error);
				}
			}
		}

		static void Espresso0_Publisher(ZContext context) 
		{
			// The publisher sends random messages starting with A-J:
			using (var publisher = new ZSocket(context, ZSocketType.PUB))
			{
				publisher.Bind("tcp://*:6000");

				var rnd = new Random();
				ZError error;
				while (true)
				{
					var s = String.Format("{0}-{1,0:D5}", (char)(rnd.Next(10) + 'A'), rnd.Next(100000));
					if (!publisher.SendFrame(new ZFrame(s), out error))
					{
						if (error == ZError.ETERM)
							return; // Interrupted
						throw new ZException(error);
					}

					Thread.Sleep(100); // Wait for 1/10th second
				}
			}
		}

		// The subscriber thread requests messages starting with
		// A and B, then reads and counts incoming messages.
		static void Espresso0_Subscriber(ZContext context) 
		{
			// Subscrie to "A" and "B"
			using (var subscriber = new ZSocket(context, ZSocketType.SUB))
			{
				subscriber.Connect("tcp://127.0.0.1:6001");
				subscriber.Subscribe("A");
				subscriber.Subscribe("B");

				ZError error;
				ZFrame frm;
				int count = 0;
				while (count < 5)
				{
					if (null == (frm = subscriber.ReceiveFrame(out error)))
					{
						if (error == ZError.ETERM)
							return;	// Interrupted
						throw new ZException(error);
					}
					++count;
				}

				Console.WriteLine("I: subscriber counted {0}", count);
			}
		}

		static void Espresso0_Listener(ZContext context) 
		{
			// The listener receives all messages flowing through the proxy, on its
			// pipe. In CZMQ, the pipe is a pair of ZMQ_PAIR sockets that connect
			// attached child threads. In other languages your mileage may vary:

			using (var listener = new ZSocket(context, ZSocketType.PAIR))
			{
				listener.Connect("inproc://listener");

				//Print everything that arrives on pipe
				ZError error;
				ZFrame frame;
				while (true)
				{
					if (null != (frame = listener.ReceiveFrame(out error)))
					{
						using (frame)
							frame.DumpZfrm();
					}
					else
					{
						if (error == ZError.ETERM)
							return;	// Interrupted
						throw new ZException(error);
					}
				}
			}
		}
	}
}