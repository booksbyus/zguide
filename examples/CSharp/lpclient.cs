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
		//
		// Lazy Pirate client
		// Use zmq_poll (pollItem.PollIn) to do a safe request-reply
		// To run, start lpserver and then randomly kill/restart it
		//
		// Author: metadings
		//

		static TimeSpan LPClient_RequestTimeout = TimeSpan.FromMilliseconds(2000);
		static int LPClient_RequestRetries = 3;

		static ZSocket LPClient_CreateZSocket(ZContext context, string name, out ZError error)
		{
			// Helper function that returns a new configured socket
			// connected to the Lazy Pirate queue

			var requester = new ZSocket(context, ZSocketType.REQ);
			requester.IdentityString = name;
			requester.Linger = TimeSpan.FromMilliseconds(1);

			if (!requester.Connect("tcp://127.0.0.1:5555", out error))
			{
				return null;
			}
			return requester;
		}

		public static void LPClient(string[] args)
		{
			if (args == null || args.Length < 1)
			{
				Console.WriteLine();
				Console.WriteLine("Usage: ./{0} LPClient [Name]", AppDomain.CurrentDomain.FriendlyName);
				Console.WriteLine();
				Console.WriteLine("    Name   Your name. Default: People");
				Console.WriteLine();
				args = new string[] { "People" };
			}

			string name = args[0];

			using (var context = new ZContext())
			{
				ZSocket requester = null;
				try
				{ // using (requester)

					ZError error;

					if (null == (requester = LPClient_CreateZSocket(context, name, out error)))
					{
						if (error == ZError.ETERM)
							return;	// Interrupted
						throw new ZException(error);
					}

					int sequence = 0;
					int retries_left = LPClient_RequestRetries;
					var poll = ZPollItem.CreateReceiver();

					while (retries_left > 0)
					{
						// We send a request, then we work to get a reply
						using (var outgoing = ZFrame.Create(4))
						{
							outgoing.Write(++sequence);
							if (!requester.Send(outgoing, out error))
							{
								if (error == ZError.ETERM)
									return;	// Interrupted
								throw new ZException(error);
							}
						}

						ZMessage incoming;
						while (true)
						{
							// Here we process a server reply and exit our loop
							// if the reply is valid.

							// If we didn't a reply, we close the client socket
							// and resend the request. We try a number of times
							// before finally abandoning:

							// Poll socket for a reply, with timeout
							if (requester.PollIn(poll, out incoming, out error, LPClient_RequestTimeout))
							{
								using (incoming)
								{
									// We got a reply from the server
									int incoming_sequence = incoming[0].ReadInt32();
									if (sequence == incoming_sequence)
									{
										Console.WriteLine("I: server replied OK ({0})", incoming_sequence);
										retries_left = LPClient_RequestRetries;
										break;
									}
									else
									{
										Console_WriteZMessage("E: malformed reply from server", incoming);
									}
								}
							}
							else
							{
								if (error == ZError.EAGAIN)
								{
									if (--retries_left == 0)
									{
										Console.WriteLine("E: server seems to be offline, abandoning");
										break;
									}

									Console.WriteLine("W: no response from server, retrying...");

									// Old socket is confused; close it and open a new one
									requester.Dispose();
									if (null == (requester = LPClient_CreateZSocket(context, name, out error)))
									{
										if (error == ZError.ETERM)
											return;	// Interrupted
										throw new ZException(error);
									}

									Console.WriteLine("I: reconnected");

									// Send request again, on new socket
									using (var outgoing = ZFrame.Create(4))
									{
										outgoing.Write(sequence);
										if (!requester.Send(outgoing, out error))
										{
											if (error == ZError.ETERM)
												return;	// Interrupted
											throw new ZException(error);
										}
									}

									continue;
								}

								if (error == ZError.ETERM)
									return;	// Interrupted
								throw new ZException(error);
							}
						}
					}
				}
				finally
				{
					if (requester != null)
					{
						requester.Dispose();
						requester = null;
					}
				}
			}
		}
	}
}