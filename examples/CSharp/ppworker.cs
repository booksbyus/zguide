using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

using ZeroMQ;

namespace Examples
{
	using PP;

	//
	// Paranoid Pirate worker
	//
	// We have a single task that implements the worker side of the
	// Paranoid Pirate Protocol (PPP). The interesting parts here are
	// the heartbeating, which lets the worker detect if the queue has
	// died, and vice versa:
	//
	// Author: metadings
	//

	static partial class Program
	{
		static ZSocket PPWorker_CreateZSocket(ZContext context, string name, out ZError error)
		{
			// Helper function that returns a new configured socket
			// connected to the Paranoid Pirate queue

			var worker = new ZSocket(context, ZSocketType.DEALER);
			worker.IdentityString = name;

			if (!worker.Connect("tcp://127.0.0.1:5556", out error))
			{
				return null;	// Interrupted
			}

			// Tell queue we're ready for work
			using (var outgoing = new ZFrame(Worker.PPP_READY))
			{
				worker.Send(outgoing);
			}

			Console.WriteLine("I:        worker ready");
			return worker;
		}

		public static void PPWorker(string[] args)
		{
			if (args == null || args.Length == 0)
			{
				Console.WriteLine();
				Console.WriteLine("Usage: ./{0} PPWorker [Name]", AppDomain.CurrentDomain.FriendlyName);
				Console.WriteLine();
				Console.WriteLine("    Name   Your name. Default: World");
				Console.WriteLine();
				args = new string[] { "World" };
			}
			string name = args[0];

			ZError error;
			using (var context = new ZContext())
			{
				ZSocket worker = null;
				try // using (worker)
				{
					if (null == (worker = PPWorker_CreateZSocket(context, name, out error)))
					{
						if (error == ZError.ETERM)
							return;	// Interrupted
						throw new ZException(error);
					}

					// If liveness hits zero, queue is considered disconnected
					int liveness = Worker.PPP_HEARTBEAT_LIVENESS;
					int interval = Worker.PPP_INTERVAL_INIT;

					// Send out heartbeats at regular intervals
					DateTime heartbeat_at = DateTime.UtcNow + Worker.PPP_HEARTBEAT_INTERVAL;

					ZMessage incoming;
					int cycles = 0;
					var poll = ZPollItem.CreateReceiver();
					var rnd = new Random();

					while (true)
					{
						if (worker.PollIn(poll, out incoming, out error, Worker.PPP_TICK))
						{
							// Get message
							// - 3-part envelope + content -> request
							// - 1-part HEARTBEAT -> heartbeat
							using (incoming)
							{
								// To test the robustness of the queue implementation we
								// simulate various typical problems, such as the worker
								// crashing or running very slowly. We do this after a few
								// cycles so that the architecture can get up and running
								// first:
								if (incoming.Count >= 3)
								{
									Console_WriteZMessage("I: receiving reply", incoming);

									cycles++;
									if (cycles > 3 && rnd.Next(5) == 0)
									{
										Console.WriteLine("I: simulating a crash");
										return;
									}
									if (cycles > 3 && rnd.Next(3) == 0)
									{
										Console.WriteLine("I: simulating CPU overload");
										Thread.Sleep(100);
									}

									Thread.Sleep(1);	// Do some heavy work

									Console.WriteLine("I: sending reply");
									worker.Send(incoming);

									liveness = Worker.PPP_HEARTBEAT_LIVENESS;
								}
								// When we get a heartbeat message from the queue, it means the
								// queue was (recently) alive, so we must reset our liveness
								// indicator:
								else if (incoming.Count == 1)
								{
									string identity = incoming[0].ReadString();

									if (identity == Worker.PPP_HEARTBEAT)
									{
										Console.WriteLine("I: receiving heartbeat");
										liveness = Worker.PPP_HEARTBEAT_LIVENESS;
									}
									else
									{
										Console_WriteZMessage("E: invalid message", incoming);
									}
								}
								else
								{
									Console_WriteZMessage("E: invalid message", incoming);
								}
							}
							interval = Worker.PPP_INTERVAL_INIT;
						}
						else
						{
							if (error == ZError.ETERM)
								break;	// Interrupted
							if (error != ZError.EAGAIN)
								throw new ZException(error);
						}

						if (error == ZError.EAGAIN)
						{
							// If the queue hasn't sent us heartbeats in a while, destroy the
							// socket and reconnect. This is the simplest most brutal way of
							// discarding any messages we might have sent in the meantime:
							if (--liveness == 0)
							{
								Console.WriteLine("W: heartbeat failure, can't reach queue");
								Console.WriteLine("W: reconnecting in {0} ms", interval);
								Thread.Sleep(interval);

								if (interval < Worker.PPP_INTERVAL_MAX)
								{
									interval *= 2;
								}
								else {
									Console.WriteLine("E: interrupted");
									break;
								}

								worker.Dispose();
								if (null == (worker = PPWorker_CreateZSocket(context, name, out error)))
								{
									if (error == ZError.ETERM)
										break;	// Interrupted
									throw new ZException(error);
								}
								liveness = Worker.PPP_HEARTBEAT_LIVENESS;
							}
						}

						// Send heartbeat to queue if it's time
						if (DateTime.UtcNow > heartbeat_at) 
						{
							heartbeat_at = DateTime.UtcNow + Worker.PPP_HEARTBEAT_INTERVAL;

							Console.WriteLine("I:   sending heartbeat");
							using (var outgoing = new ZFrame(Worker.PPP_HEARTBEAT))
							{
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