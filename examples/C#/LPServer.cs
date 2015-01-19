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
		// Lazy Pirate server
		// Binds REP socket to tcp://*:5555
		// Like hwserver except:
		// - echoes request as-is
		// - randomly runs slowly, or exits to simulate a crash.

		public static void LPServer(IDictionary<string, string> dict, string[] args)
		{
			using (var context = ZContext.Create())
			using (var responder = ZSocket.Create(context, ZSocketType.REP))
			{
				responder.Bind("tcp://*:5555");

				ZError error;
				int cycles = 0;
				var rnd = new Random();

				while (true)
				{
					ZMessage incoming;
					if (null == (incoming = responder.ReceiveMessage(out error)))
					{
						if (error == ZError.ETERM)
							return;

						throw new ZException(error);
					}
					using (incoming)
					{
						++cycles;

						// Simulate various problems, after a few cycles
						if (cycles > 3 && rnd.Next(3) == 0)
						{
							Console.WriteLine("I: simulating a crash");
							break;
						}
						else if (cycles > 3 && rnd.Next(3) == 0)
						{
							Console.WriteLine("I: simulating CPU overload");
							Thread.Sleep(2);
						}

						Console.WriteLine("I: normal request ({0})", incoming[0].ReadInt32());
						Thread.Sleep(1); // Do some heavy work
						responder.SendMessage(incoming);
					}
				}
			}
		}
	}
}