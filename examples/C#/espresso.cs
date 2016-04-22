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
		public static void Espresso(string[] args)
		{
			//
			// Espresso Pattern
			// This shows how to capture data using a pub-sub proxy
			//
			// Author: metadings
			//

			using (var context = new ZContext())
			using (var subscriber = new ZSocket(context, ZSocketType.XSUB))
			using (var publisher = new ZSocket(context, ZSocketType.XPUB))
			using (var listener = new ZSocket(context, ZSocketType.PAIR))
			{
				new Thread(() => Espresso_Publisher(context)).Start();
				new Thread(() => Espresso_Subscriber(context)).Start();
				new Thread(() => Espresso_Listener(context)).Start();

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

		static void Espresso_Publisher(ZContext context) 
		{
			// The publisher sends random messages starting with A-J:

			using (var publisher = new ZSocket(context, ZSocketType.PUB))
			{
				publisher.Bind("tcp://*:6000");

				ZError error;

				while (true)
				{
					var frame = ZFrame.Create(8);
					var bytes = new byte[8];
					using (var rng = new System.Security.Cryptography.RNGCryptoServiceProvider())
					{
						rng.GetBytes(bytes);
					}
					frame.Write(bytes, 0, 8);
					
					if (!publisher.SendFrame(frame, out error))
					{
						if (error == ZError.ETERM)
							return;	// Interrupted
						throw new ZException(error);
					}

					Thread.Sleep(1);
				}
			}
		}

		static void Espresso_Subscriber(ZContext context) 
		{
			// The subscriber thread requests messages starting with
			// A and B, then reads and counts incoming messages.

			using (var subscriber = new ZSocket(context, ZSocketType.SUB))
			{
				subscriber.Connect("tcp://127.0.0.1:6001");
				subscriber.Subscribe("A");
				subscriber.Subscribe("B");

				ZError error;
				ZFrame frame;
				int count = 0;
				while (count < 5)
				{
					if (null == (frame = subscriber.ReceiveFrame(out error)))
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

		static void Espresso_Listener(ZContext context) 
		{
			// The listener receives all messages flowing through the proxy, on its
			// pipe. In CZMQ, the pipe is a pair of ZMQ_PAIR sockets that connect
			// attached child threads. In other languages your mileage may vary:

			using (var listener = new ZSocket(context, ZSocketType.PAIR))
			{
				listener.Connect("inproc://listener");

				ZError error;
				ZFrame frame;
				while (true)
				{
					if (null != (frame = listener.ReceiveFrame(out error)))
					{
						using (frame)
						{
							byte first = frame.ReadAsByte();

							var rest = new byte[9];
							frame.Read(rest, 0, rest.Length);

							Console.WriteLine("{0} {1}", (char)first, rest.ToHexString());

							if (first == 0x01)
							{
								// Subscribe
							}
							else if (first == 0x00)
							{
								// Unsubscribe
								context.Shutdown();
							}
						}
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