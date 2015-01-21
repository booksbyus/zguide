using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

using ZeroMQ;

namespace ZeroMQ.Test
{
	// Paranoid Pirate worker

	// We have a single task that implements the worker side of the
	// Paranoid Pirate Protocol (PPP). The interesting parts here are
	// the heartbeating, which lets the worker detect if the queue has
	// died, and vice versa:

	static partial class Program
	{

		public const int PPP_INTERVAL_INIT = 1000;
		public const int PPP_INTERVAL_MAX = 32000;

		public static ZSocket PPWorker_CreateSocket(ZContext context, string name)
		{
			// Helper function that returns a new configured socket
			// connected to the Paranoid Pirate queue

			var worker = ZSocket.Create(context, ZSocketType.DEALER);
			worker.IdentityString = name;
			worker.Connect("tcp://127.0.0.1:5556");

			// Tell queue we're ready for work
			using (var outgoing = new ZMessage())
			{
				// outgoing.Add(new ZFrame(name));
				outgoing.Add(new ZFrame());
				outgoing.Add(new ZFrame(PPP_READY));

				worker.Send(outgoing);
			}
			Console.WriteLine("I: worker ready");

			return worker;
		}

		public static void PPWorker(IDictionary<string, string> dict, string[] args)
		{
			if (args == null || args.Length == 0)
			{
				args = new string[] { "World" };
			}
			string name = args[0];

			using (var context = ZContext.Create())
			{
				ZSocket worker = null;
				try // using (worker)
				{
					worker = PPWorker_CreateSocket(context, name);

					// If liveness hits zero, queue is considered disconnected
					int liveness = PPP_HEARTBEAT_LIVENESS;
					int interval = PPP_INTERVAL_INIT;

					// Send out heartbeats at regular intervals
					DateTime heartbeat_at = DateTime.UtcNow + PPP_HEARTBEAT_INTERVAL;

					ZError error;
					ZMessage incoming;

					var poll = ZPollItem.CreateReceiver(worker);

					int cycles = 0;
					var rnd = new Random();
					while (true)
					{
						if (poll.PollIn(out incoming, out error, PPP_HEARTBEAT_INTERVAL))
						{
							// Get message
							// - 3-part envelope + content -> request
							// - 1-part HEARTBEAT -> heartbeat
							using (incoming)
							{
								incoming.RemoveAt(0);

								// To test the robustness of the queue implementation we //
								// simulate various typical problems, such as the worker
								// crashing or running very slowly. We do this after a few
								// cycles so that the architecture can get up and running
								// first:
								if (incoming.Count >= 3)
								{
									cycles++;
									if (cycles > 3 && rnd.Next(5) == 0)
									{
										Console.WriteLine("I: simulating a crash");
										return;
									}
									if (cycles > 3 && rnd.Next(5) == 0)
									{
										Console.WriteLine("I: simulating CPU overload");
										Thread.Sleep(300);
									}

									Thread.Sleep(1);	// Do some heavy work

									incoming.Prepend(new ZFrame());

									Console.WriteLine("I: reply: {0}, {1}, {2}, {3}, {4}, {5}",
										incoming.Count > 0 ? incoming[0].ReadString() : null,
										incoming.Count > 1 ? incoming[1].ReadString() : null,
										incoming.Count > 2 ? incoming[2].ReadString() : null,
										incoming.Count > 3 ? incoming[3].ReadString() : null,
										incoming.Count > 4 ? incoming[4].ReadString() : null,
										incoming.Count > 5 ? incoming[5].ReadString() : null
									);
									worker.Send(incoming);

									liveness = PPP_HEARTBEAT_LIVENESS;
								}
								// When we get a heartbeat message from the queue, it means the
								// queue was (recently) alive, so we must reset our liveness
								// indicator:
								else if (incoming.Count == 1)
								{
									string identity = incoming[0].ReadString();

									if (identity == PPP_HEARTBEAT)
									{
										Console.WriteLine("I: heartbeating");
										liveness = PPP_HEARTBEAT_LIVENESS;
									}
									else
									{
										Console.WriteLine("E: invalid message: {0}", identity);
									}
								}
								else
								{
									Console.WriteLine("E: invalid message: {0}, {1}, {2}, {3}",
										incoming.Count > 0 ? incoming[0].ReadString() : null, 
										incoming.Count > 1 ? incoming[1].ReadString() : null,
										incoming.Count > 2 ? incoming[2].ReadString() : null,
										incoming.Count > 3 ? incoming[3].ReadString() : null
									);
								}
							}
							interval = PPP_INTERVAL_INIT;
						}
						else
						{
							if (error == ZError.ETERM)
								return;
							if (error != ZError.EAGAIN)
								throw new ZException(error);
						}

						// If the queue hasn't sent us heartbeats in a while, destroy the
						// socket and reconnect. This is the simplest most brutal way of
						// discarding any messages we might have sent in the meantime:
						if (--liveness == 0)
						{
							Console.WriteLine("W: heartbeat failure, can't reach queue");
							Console.WriteLine("W: reconnecting in {0} ms", interval);
							Thread.Sleep(interval);

							if (interval < PPP_INTERVAL_MAX)
							{
								interval *= 2;
							}
							else 
								return;

							worker.Dispose();
							worker = PPWorker_CreateSocket(context, name);
							liveness = PPP_HEARTBEAT_LIVENESS;
						}
						// Send heartbeat to queue if it's time
						if (DateTime.UtcNow > heartbeat_at) 
						{
							heartbeat_at = DateTime.UtcNow + PPP_HEARTBEAT_INTERVAL;
							Console.WriteLine("I: sending worker heartbeat");
							using (var outgoing = new ZMessage()) 
							{
								// outgoing.Add(new ZFrame(name));
								outgoing.Add(new ZFrame());
								outgoing.Add(new ZFrame(PPP_HEARTBEAT));

								worker.Send(outgoing);
							}
						}
					}
				}
				finally
				{
					if (worker != null)
					{
						worker.Dispose();
						worker = null;
					}
				}
			}
		}
	}
}