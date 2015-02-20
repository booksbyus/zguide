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
		public static void Interrupt(string[] args)
		{
			//
			// Interrupt
			//
			// Author: metadings
			//

			if (args == null || args.Length == 0)
			{
				args = new string[] { "World" };
			}

			string name = args[0];

			var error = default(ZError);

			using (var context = new ZContext())
			using (var responder = new ZSocket(context, ZSocketType.REP))
			{
				Console.CancelKeyPress += (s, ea) => 
				{ 
					ea.Cancel = true;
					context.Shutdown(); 
				};

				responder.Bind("tcp://*:5555");

				ZFrame request;

				while (true)
				{
					if (Console.KeyAvailable)
					{
						ConsoleKeyInfo info = Console.ReadKey(true);
						/* if (info.Modifiers == ConsoleModifiers.Control && info.Key == ConsoleKey.C)
						{
							context.Shutdown();
						} /**/
						if (info.Key == ConsoleKey.Escape)
						{
							context.Shutdown();
						}
					}

					if (null == (request = responder.ReceiveFrame(ZSocketFlags.DontWait, out error)))
					{
						if (error == ZError.EAGAIN)
						{
							Thread.Sleep(1);
							continue;
						}
						if (error == ZError.ETERM)
							break;	// Interrupted
						throw new ZException(error);
					}

					using (request)
					{
						Console.Write("Received: {0}!", request.ReadString());

						Thread.Sleep(512);	// See also the much slower reaction

						Console.WriteLine(" Sending {0}... ", name);

						if (!responder.Send(new ZFrame(name), out error))
						{
							if (error == ZError.ETERM)
								break;	// Interrupted
							throw new ZException(error);
						}
					}
				}

				if (error == ZError.ETERM)
				{
					Console.WriteLine("Terminated! You have pressed CTRL+C or ESC.");
					return;
				}
				throw new ZException(error);
			}
		}
	}
}