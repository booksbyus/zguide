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

		public static void LPClient(IDictionary<string, string> dict, string[] args)
		{
			if (args == null || args.Length < 1)
			{
				args = new string[] { "People" };
			}
			string name = args[0];

			using (var context = ZContext.Create())
			{
				ZSocket requester = null;
				try { // using (requester)
					requester = ZSocket.Create(context, ZSocketType.REQ);
					requester.Identity = Encoding.UTF8.GetBytes(name);
					requester.Connect("tcp://127.0.0.1:5555");

					var poller = ZPollItem.CreateReceiver(requester);

					int sequence = 0;
					int retries_left = LPClient_RequestRetries;

					while (retries_left > 0)
					{
						// We send a request, then we work to get a reply
						using (var outgoing = ZFrame.Create(4))
						{
							outgoing.Write(++sequence);

							requester.Send(outgoing);
						}

						ZError error;
						ZMessage incoming;

						while (true)
						{
							// Poll socket for a reply, with timeout
							if (!poller.PollIn(out incoming, out error, LPClient_RequestTimeout))
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

									Console.WriteLine("I: reconnecting to server...");
									requester = ZSocket.Create(context, ZSocketType.REQ);
									requester.Identity = Encoding.UTF8.GetBytes(name);
									requester.Connect("tcp://127.0.0.1:5555");

									// Send request again, on new socket
									continue;
								}

								if (error == ZError.ETERM)
									return;

								throw new ZException(error);
							}
							using (incoming)
							{
								int incoming_sequence = incoming[0].ReadInt32();
								Console.WriteLine("I: server replied OK ({0})", incoming_sequence);

								// Request the next one
								retries_left = LPClient_RequestRetries;
								break;
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