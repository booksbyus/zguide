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
		static TimeSpan LPClient_RequestTimeout = TimeSpan.FromMilliseconds(2500);
		static int LPClient_RequestRetries = 3;

		static ZSocket LPClient_CreateZSocket(ZContext context, string name, out ZPollItem pollItem, out ZError error)
		{
			// Helper function that returns a new configured socket
			// connected to the Lazy Pirate queue

			var requester = ZSocket.Create(context, ZSocketType.REQ);
			requester.IdentityString = name;

			if (!requester.Connect("tcp://127.0.0.1:5555", out error))
			{
				pollItem = null;
				return null;
			}

			pollItem = ZPollItem.CreateReceiver(requester);
			return requester;
		}

		public static void LPClient(IDictionary<string, string> dict, string[] args)
		{
			// Lazy Pirate client
			// Use zmq_poll (pollItem.PollIn) to do a safe request-reply
			// To run, start lpserver and then randomly kill/restart it

			if (args == null || args.Length < 1)
			{
				args = new string[] { "People" };
			}

			string name = args[0];

			ZError error;
			using (var context = ZContext.Create())
			{
				ZSocket requester = null;
				try { // using (requester)

					int sequence = 0;
					int retries_left = LPClient_RequestRetries;
					while (retries_left > 0)
					{
						ZPollItem pollItem;
						if (null == (requester = LPClient_CreateZSocket(context, name, out pollItem, out error)))
						{
							if (error == ZError.ETERM)
								return;	// Interrupted
							throw new ZException(error);
						}

						// We send a request, then we work to get a reply
						using (var outgoing = ZFrame.Create(4))
						{
							outgoing.Write(++sequence);
							requester.Send(outgoing);
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
							if (pollItem.PollIn(out incoming, out error, LPClient_RequestTimeout))
							{
								using (incoming)
								{
									// We got a reply from the server
									int incoming_sequence = incoming[0].ReadInt32();
									Console.WriteLine("I: server replied OK ({0})", incoming_sequence);

									// Request the next one
									retries_left = LPClient_RequestRetries;
									break;
								}
							}
							else 
							{
								if (error == ZError.EAGAIN)
								{
									if (--retries_left == 0)
									{
										Console.WriteLine("E: server seems to be offline, abandoning");
										return;
									}
									Console.WriteLine("W: no response from server, retrying...");

									// Old socket is confused; close it and open a new one
									requester.Dispose();
									requester = null;

									break; // Send request again, on new socket
								}

								if (error == ZError.ETERM)
									return;

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