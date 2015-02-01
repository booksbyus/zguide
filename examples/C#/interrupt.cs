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
		public static void Interrupt(IDictionary<string, string> dict, string[] args)
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

			using (var context = new ZContext())
			using (var responder = new ZSocket(context, ZSocketType.REP))
			{
				Console.CancelKeyPress += (s, ea) => 
				{ 
					ea.Cancel = true;
					context.Shutdown(); 
				};

				responder.Bind("tcp://*:5555");

				var error = ZError.None;
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
					} /**/

					if (null == (request = responder.ReceiveFrame(ZSocketFlags.DontWait, out error)))
					{
						if (error == ZError.EAGAIN)
						{
							error = ZError.None;
							Thread.Sleep(512);	// See also the much slower reaction

							continue;
						} /**/
						if (error == ZError.ETERM)
							break;	// Interrupted
						throw new ZException(error);
					}

					using (request)
					{
						Console.Write("Received: {0}!", request.ReadString());

						Console.WriteLine(" Sending {0}... ", name);
						using (var response = new ZFrame(name))
						{
							if (!responder.Send(response, out error))
							{
								if (error == ZError.ETERM)
									break;	// Interrupted
								throw new ZException(error);
							}
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