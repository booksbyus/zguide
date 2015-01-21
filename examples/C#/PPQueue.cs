using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

using ZeroMQ;

namespace ZeroMQ.Test
{
	using PP;

	// Paranoid Pirate queue

	// Here we define the worker class; a structure and a set of functions that
	// act as constructor, destructor, and methods on worker objects:

	namespace PP
	{
		public partial class Worker : IDisposable
		{
			public ZFrame Identity;

			public DateTime Expiry;

			public string IdentityString {
				get {
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

				this.Expiry = DateTime.UtcNow + new TimeSpan(
					Program.PPP_HEARTBEAT_INTERVAL.Milliseconds * Program.PPP_HEARTBEAT_LIVENESS
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
				var old = workers.FirstOrDefault(
					w => worker.IdentityString == w.IdentityString
				);
				if (old != null)
				{
					workers.Remove(old);
					old.Dispose();
				}
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

		// 3-5 is reasonable
		public static readonly int PPP_HEARTBEAT_LIVENESS = 5;
		public static readonly TimeSpan PPP_HEARTBEAT_INTERVAL = TimeSpan.FromMilliseconds(1000);

		public const string PPP_READY = "READY";
		public const string PPP_HEARTBEAT = "HEARTBEAT";


		public static void PPQueue(IDictionary<string, string> dict, string[] args)
		{
			using (var context = ZContext.Create())
			using (var frontend = ZSocket.Create(context, ZSocketType.ROUTER))
			using (var backend = ZSocket.Create(context, ZSocketType.ROUTER))
			{
				frontend.Bind("tcp://*:5555");
				backend.Bind("tcp://*:5556");

				// List of available workers
				var workers = new List<Worker>();

				// Send out heartbeats at regular intervals
				DateTime heartbeat_at = DateTime.UtcNow + PPP_HEARTBEAT_INTERVAL;

				// Create a Receiver poll
				var pollers = new ZPollItem[]
				{
					ZPollItem.CreateReceiver(backend),
					ZPollItem.CreateReceiver(frontend)
				};

				ZError error;
				ZMessage incoming;

				while (true)
				{
					// Handle worker activity on backend
					if (pollers[0].PollIn(out incoming, out error, PPP_HEARTBEAT_INTERVAL))
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
								if (message == PPP_READY)
								{
									Console.WriteLine("I: worker ready");
								}
								else if (message == PPP_HEARTBEAT)
								{
									Console.WriteLine("I: worker heartbeat");
								}
								else
								{
									Console.WriteLine("E: invalid message from worker: {0}", message);
								}
							}
							else
							{
								frontend.Send(incoming);
							}
						}
					}
					else
					{
						if (error == ZError.ETERM)
							return;
						if (error != ZError.EAGAIN)
							throw new ZException(error);
					}

					// Handle worker activity on frontend
					if (workers.Count > 0)
					{
						// Poll frontend only if we have available workers
						if (pollers[1].PollIn(out incoming, out error, PPP_HEARTBEAT_INTERVAL))
						{
							// Now get next client request, route to next worker
							using (incoming)
							{
								incoming.Wrap(workers.Next());

								Console.WriteLine("I: route to next worker");
								backend.Send(incoming);
							}
						}
						else
						{
							if (error == ZError.ETERM)
								return;
							if (error != ZError.EAGAIN)
								throw new ZException(error);
						}
					}

					// We handle heartbeating after any socket activity. First, we send
					// heartbeats to any idle workers if it's time. Then, we purge any
					// dead workers:
					if (DateTime.UtcNow > heartbeat_at)
					{
						foreach (Worker worker in workers)
						{
							using (var outgoing = new ZMessage())
							{
								outgoing.Add(worker.Identity);
								outgoing.Add(new ZFrame());
								outgoing.Add(new ZFrame(PPP_HEARTBEAT));

								backend.Send(outgoing);
							}
						}
						heartbeat_at = DateTime.UtcNow + PPP_HEARTBEAT_INTERVAL;
					}
					workers.Purge();

				} /**/
			}
		}
	}
}