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
	// Paranoid Pirate queue
	//
	// Author: metadings
	//

	// Here we define the worker class; a structure and a set of functions that
	// act as constructor, destructor, and methods on worker objects:

	namespace PP
	{
		public class Worker : IDisposable
		{
			public const int PPP_HEARTBEAT_LIVENESS = 3; // 3-5 is reasonable
			public static readonly TimeSpan PPP_HEARTBEAT_INTERVAL = TimeSpan.FromMilliseconds(500);
			public static readonly TimeSpan PPP_TICK = TimeSpan.FromMilliseconds(250);

			public const string PPP_READY = "READY";
			public const string PPP_HEARTBEAT = "HEARTBEAT";

			public const int PPP_INTERVAL_INIT = 1000;
			public const int PPP_INTERVAL_MAX = 32000;

			public ZFrame Identity;

			public DateTime Expiry;

			public string IdentityString {
				get {
					Identity.Position = 0;
					return Identity.ReadString();
				}
				set {
					if (Identity != null)
					{
						Identity.Dispose();
					}
					Identity = new ZFrame(value);
				}
			}

			// Construct new worker
			public Worker(ZFrame identity) 
			{
				Identity = identity;

				this.Expiry = DateTime.UtcNow + TimeSpan.FromMilliseconds(
					PPP_HEARTBEAT_INTERVAL.Milliseconds * PPP_HEARTBEAT_LIVENESS
				);
			}

			// Destroy specified worker object, including identity frame.
			public void Dispose()
			{
				GC.SuppressFinalize(this);
				Dispose(true);
			}

			protected void Dispose(bool disposing)
			{
				if (disposing)
				{
					if (Identity != null)
					{
						Identity.Dispose();
						Identity = null;
					}
				}
			}
		}

		public static class Workers
		{
			public static void Ready(this IList<Worker> workers, Worker worker) 
			{
				workers.Add(worker);
			}

			public static ZFrame Next(this IList<Worker> workers) 
			{
				Worker worker = workers[0];
				workers.RemoveAt(0);

				ZFrame identity = worker.Identity;
				worker.Identity = null;
				worker.Dispose();

				return identity;
			}

			public static void Purge(this IList<Worker> workers) 
			{
				foreach (Worker worker in workers.ToList())
				{
					if (DateTime.UtcNow < worker.Expiry)
						continue;	// Worker is alive, we're done here

					workers.Remove(worker);
				}
			}
		}
	}

	static partial class Program
	{
		public static void PPQueue(string[] args)
		{
			using (var context = new ZContext())
			using (var backend = new ZSocket(context, ZSocketType.ROUTER))
			using (var frontend = new ZSocket(context, ZSocketType.ROUTER))
			{
				backend.Bind("tcp://*:5556");
				frontend.Bind("tcp://*:5555");

				// List of available workers
				var workers = new List<Worker>();

				// Send out heartbeats at regular intervals
				DateTime heartbeat_at = DateTime.UtcNow + Worker.PPP_HEARTBEAT_INTERVAL;

				// Create a Receiver ZPollItem (ZMQ_POLLIN)
				var poll = ZPollItem.CreateReceiver();

				ZError error;
				ZMessage incoming;
				while (true)
				{
					// Handle worker activity on backend
					if (backend.PollIn(poll, out incoming, out error, Worker.PPP_TICK))
					{
						using (incoming)
						{
							// Any sign of life from worker means it's ready
							ZFrame identity = incoming.Unwrap();
							var worker = new Worker(identity);
							workers.Ready(worker);

							// Validate control message, or return reply to client
							if (incoming.Count == 1)
							{
								string message = incoming[0].ReadString();
								if (message == Worker.PPP_READY)
								{
									Console.WriteLine("I:        worker ready ({0})", worker.IdentityString);
								}
								else if (message == Worker.PPP_HEARTBEAT)
								{
									Console.WriteLine("I: receiving heartbeat ({0})", worker.IdentityString);
								}
								else
								{
									Console_WriteZMessage("E: invalid message from worker", incoming);
								}
							}
							else
							{
								if (Verbose) Console_WriteZMessage("I: [backend sending to frontend] ({0})", incoming, worker.IdentityString);
								frontend.Send(incoming);
							}
						}
					}
					else
					{
						if (error == ZError.ETERM)
							break;	// Interrupted
						if (error != ZError.EAGAIN)
							throw new ZException(error);
					}

					// Handle client activity on frontend
					if (workers.Count > 0)
					{
						// Poll frontend only if we have available workers
						if (frontend.PollIn(poll, out incoming, out error, Worker.PPP_TICK))
						{
							// Now get next client request, route to next worker
							using (incoming)
							{
								ZFrame workerIdentity = workers.Next();
								incoming.Prepend(workerIdentity);

								if (Verbose) Console_WriteZMessage("I: [frontend sending to backend] ({0})", incoming, workerIdentity.ReadString());
								backend.Send(incoming);
							}
						}
						else
						{
							if (error == ZError.ETERM)
								break;	// Interrupted
							if (error != ZError.EAGAIN)
								throw new ZException(error);
						}
					}

					// We handle heartbeating after any socket activity. First, we send
					// heartbeats to any idle workers if it's time. Then, we purge any
					// dead workers:
					if (DateTime.UtcNow > heartbeat_at)
					{
						heartbeat_at = DateTime.UtcNow + Worker.PPP_HEARTBEAT_INTERVAL;

						foreach (Worker worker in workers)
						{
							using (var outgoing = new ZMessage())
							{
								outgoing.Add(ZFrame.CopyFrom(worker.Identity));
								outgoing.Add(new ZFrame(Worker.PPP_HEARTBEAT));

								Console.WriteLine("I:   sending heartbeat ({0})", worker.IdentityString);
								backend.Send(outgoing);
							}
						}
					}
					workers.Purge();
				}

				// When we're done, clean up properly
				foreach (Worker worker in workers)
				{
					worker.Dispose();
				}
			}
		}
	}
}