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
		public static void Espresso(IDictionary<string, string> dict, string[] args)
		{
			//
			// Espresso Pattern
			// This shows how to capture data using a pub-sub proxy
			//
			// Authors: Pieter Hintjens, Uli Riehm
			//

			using (var context = ZContext.Create())
			using (var subscriber = ZSocket.Create(context, ZSocketType.XSUB))
			using (var publisher = ZSocket.Create(context, ZSocketType.XPUB))
			using (var listener = ZSocket.Create(context, ZSocketType.PAIR))
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

			using (var publisher = ZSocket.Create(context, ZSocketType.PUB))
			{
				publisher.Bind("tcp://*:6000");

				ZError error;
				var hash = new System.Security.Cryptography.RNGCryptoServiceProvider();

				while (true)
				{
					var bytes = new byte[5];
					hash.GetBytes(bytes);

					if (!publisher.Send(bytes, 0, bytes.Length, ZSocketFlags.None, out error))
					{
						if (error == ZError.ETERM)
							return;	// Interrupted
						throw new ZException(error);
					}

					Thread.Sleep(20);
				}
			}
		}

		static void Espresso_Subscriber(ZContext context) 
		{
			// The subscriber thread requests messages starting with
			// A and B, then reads and counts incoming messages.

			using (var subscriber = ZSocket.Create(context, ZSocketType.SUB))
			{
				subscriber.Connect("tcp://127.0.0.1:6001");
				subscriber.Subscribe("A");
				subscriber.Subscribe("B");

				ZError error;
				int count = 0;
				while (count < 5)
				{
					var bytes = new byte[10];
					if (!subscriber.ReceiveBytes(bytes, 0, bytes.Length, ZSocketFlags.None, out error))
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

			using (var listener = ZSocket.Create(context, ZSocketType.PAIR))
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
							byte first = frame.ReadByte();

							var rest = new byte[9];
							frame.Read(rest, 0, rest.Length);

							Console.WriteLine("{0} {1}", (char)first, rest.ToHexString());
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