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
			// Authors: Pieter Hintjens, Uli Riehm
			//

			using (var context = ZContext.Create())
			using (var responder = ZSocket.Create(context, ZSocketType.REP))
			{
				Console.CancelKeyPress += (sender, e) =>
				{
					e.Cancel = false;
					// context.Terminate();
				};

				responder.Bind("tcp://*:5555");

				var error = ZError.None;
				ZFrame request;
				while (true)
				{
					if (Console.KeyAvailable)
					{
						ConsoleKeyInfo info = Console.ReadKey(true);
						if (info.Modifiers == ConsoleModifiers.Control && info.Key == ConsoleKey.C)
						{
							// error = ZError.ETERM;
							// break;
							context.Terminate();
						} 
						if (info.Key == ConsoleKey.Escape)
						{
							context.Terminate();
						}
					}

					if (null == (request = responder.ReceiveFrame(ZSocketFlags.DontWait, out error)))
					{
						if (error == ZError.EAGAIN)
						{
							error = ZError.None;
							Thread.Sleep(1);

							continue;
						}
						if (error == ZError.ETERM)
							break;	// Interrupted
						throw new ZException(error);
					}

					using (request)
					{
						string respondText = "Hello";
						Console.WriteLine("Received: {0}!", respondText, request.ReadString());

						Console.Write("Sending {0}... ", respondText);
						using (var response = new ZFrame(respondText))
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
					Console.WriteLine("Terminating, you have pressed ESC.");
				}
				else
				{
					throw new ZException(error);
				}
			}
		}
	}
}