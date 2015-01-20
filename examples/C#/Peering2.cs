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
		static int Peering2_Clients = 10;
		static int Peering2_Workers = 3;

		// Broker peering simulation (part 2)
		// Prototypes the request-reply flow

		static void Peering2_ClientTask(ZContext context, int i, string name, string message) 
		{
			// The client task does a request-reply dialog
			// using a standard synchronous REQ socket

			using (var client = ZSocket.Create(context, ZSocketType.REQ))
			{
				client.Identity = Encoding.UTF8.GetBytes(name);
				client.Connect("tcp://127.0.0.1:" + Peering2_GetPort(name) + 1);

				ZError error;

				while (true)
				{
					// Send
					using (var outgoing = new ZMessage())
					{
						outgoing.Add(new ZFrame(message));

						client.SendMessage(outgoing);
					}

					// Receive
					ZMessage incoming;
					if (null == (incoming = client.ReceiveMessage(out error)))
					{
						if (error == ZError.ETERM)
							return;	// Interrupted

						throw new ZException(error);
					}

					using (incoming)
					{
						// Do
						Console.WriteLine("Client {0}: {1}", name, incoming[0].ReadString());
					}
				}
			}
		}

		static void Peering2_WorkerTask(ZContext context, int i, string name) 
		{
			// The worker task plugs into the load-balancer using a REQ socket

			using (var worker = ZSocket.Create(context, ZSocketType.REQ))
			{
				worker.Identity = Encoding.UTF8.GetBytes(name);
				worker.Connect("tcp://127.0.0.1:" + Peering2_GetPort(name) + 2);

				// Tell broker we're ready for work
				worker.SendFrame(new ZFrame("READY"));

				// Process messages as they arrive
				ZError error;
				ZMessage incoming;
				while (true)
				{
					// Receive
					if (null == (incoming = worker.ReceiveMessage(out error)))
					{
						if (error == ZError.ETERM)
							return;	// Interrupted

						throw new ZException(error);
					}
					using (incoming)
					{
						Console.WriteLine("Worker {0}: {1}", name, incoming[0].ReadString());

						// Work
						Thread.Sleep(1);

						// Send
						using (var outgoing = new ZMessage())
						{
							outgoing.Add(new ZFrame("OK"));

							worker.SendMessage(outgoing);
						}
					}
				}
			}
		}

		// The main task begins by setting-up its frontend and backend sockets
		// and then starting its client and worker tasks:
		public static void Peering2(IDictionary<string, string> dict, string[] args)
		{
			// First argument is this broker's name
			// Other arguments are our peers' names
			//
			if (args == null || args.Length < 3)
			{
				Console.WriteLine("Usage: {0} Peering2 Hello Me You", AppDomain.CurrentDomain.FriendlyName);
				Console.WriteLine("       {0} Peering2 Message You Me", AppDomain.CurrentDomain.FriendlyName);
				return;
			}

			string message = args[0];

			string name = args[1];
			Console.WriteLine("I: preparing broker as {0}", name);

			ZError error;

			using (var context = ZContext.Create())
			using (var cloudFrontend = ZSocket.Create(context, ZSocketType.ROUTER))
			using (var cloudBackend = ZSocket.Create(context, ZSocketType.ROUTER))
			using (var localFrontend = ZSocket.Create(context, ZSocketType.ROUTER))
			using (var localBackend = ZSocket.Create(context, ZSocketType.ROUTER))
			{
				// Bind cloud frontend to endpoint
				cloudFrontend.Identity = Encoding.UTF8.GetBytes(name);
				cloudFrontend.Bind("tcp://127.0.0.1:" + Peering2_GetPort(name) + 0);

				// Connect cloud backend to all peers
				cloudBackend.Identity = Encoding.UTF8.GetBytes(name);
				for (int i = 2; i < args.Length; ++i)
				{
					string peer = args[i];
					Console.WriteLine("I: connecting to cloud frontend at {0}", peer);
					cloudBackend.Connect("tcp://127.0.0.1:" + Peering2_GetPort(peer) + 0);
				}

				// Prepare local frontend and backend
				localFrontend.Bind("tcp://127.0.0.1:" + Peering2_GetPort(name) + 1);
				localBackend.Bind("tcp://127.0.0.1:" + Peering2_GetPort(name) + 2);

				// Get user to tell us when we can start...
				Console.WriteLine("Press ENTER when all brokers are started...");
				Console.ReadKey(true);

				// Start local workers
				for (int i = 0; i < Peering2_Workers; ++i)
				{
					int j = i; new Thread(() => Peering2_WorkerTask(context, j, name)).Start();
				}

				// Start local clients
				for (int i = 0; i < Peering2_Clients; ++i)
				{
					int j = i; new Thread(() => Peering2_ClientTask(context, j, name, message)).Start();
				}

				// Here, we handle the request-reply flow. We're using load-balancing
				// to poll workers at all times, and clients only when there are one
				// or more workers available.

				// Least recently used queue of available workers
				var workers = new List<string>();
				var backends = new ZPollItem[]
				{
					ZPollItem.CreateReceiver(localBackend),
					ZPollItem.CreateReceiver(cloudBackend)
				};

				ZMessage incoming;
				TimeSpan wait;

				while (true)
				{
					// If we have no workers, wait indefinitely
					wait = workers.Count > 0 ? TimeSpan.FromMilliseconds(1000) : TimeSpan.Zero;

					// Poll localBackend
					if (backends[0].PollIn(out incoming, out error, wait))
					{
						// Handle reply from local worker
						string identity = incoming[0].ReadString();
						workers.Add(identity);

						// If it's READY, don't route the message any further
						string hello = incoming[2].ReadString();
						if (hello == "READY")
						{
							incoming.Dispose();
							incoming = null;
						}
					}
					else if (error == ZError.EAGAIN && backends[1].PollIn(out incoming, out error, wait))
					{
						// We don't use peer broker identity for anything

						// string identity = incoming[0].ReadString();

						// string ok = incoming[2].ReadString();
					}
					else
					{
						if (error == ZError.ETERM)
							return;	// Interrupted

						if (error != ZError.EAGAIN)
							throw new ZException(error);
					}

					if (incoming != null)
					{
						// Route reply to cloud if it's addressed to a broker
						string identity = incoming[0].ReadString();

						for (int i = 2; i < args.Length; ++i)
						{
							if (identity == args[i])
							{
								using (incoming)
									cloudFrontend.SendMessage(incoming);

								incoming = null;
							}
						}
					}

					// Route reply to client if we still need to
					if (incoming != null)
					{
						using (incoming)
							localFrontend.SendMessage(incoming);

						incoming = null;
					}

					// Now we route as many client requests as we have worker capacity
					// for. We may reroute requests from our local frontend, but not from //
					// the cloud frontend. We reroute randomly now, just to test things
					// out. In the next version, we'll do this properly by calculating
					// cloud capacity://

					var frontends = new ZPollItem[]
					{
						ZPollItem.CreateReceiver(localFrontend),
						ZPollItem.CreateReceiver(cloudFrontend)
					};
					var rnd = new Random();

					while (workers.Count > 0)
					{
						int reroutable = 0;

						// We'll do peer brokers first, to prevent starvation

						if (frontends[1].PollIn(out incoming, out error, TimeSpan.FromMilliseconds(64)))
						{
							reroutable = 0;
						}
						else if (error == ZError.EAGAIN && frontends[0].PollIn(out incoming, out error, TimeSpan.FromMilliseconds(64)))
						{
							reroutable = 1;
						}
						else
						{
							if (error == ZError.ETERM)
								return;	// Interrupted

							if (error == ZError.EAGAIN)
								break;	// No work, go back to backends

							throw new ZException(error);
						}

						using (incoming)
						{
							// If reroutable, send to cloud 20% of the time
							// Here we'd normally use cloud status information
							//
							if (reroutable == 1 && rnd.Next(5) == 0)
							{
								// Route to random broker peer

								int peer = rnd.Next(args.Length - 2) + 2;

								using (var outgoing = new ZMessage())
								{
									outgoing.Add(new ZFrame(args[peer]));
									outgoing.Add(new ZFrame());
									outgoing.Add(new ZFrame(incoming[2].ReadString()));

									cloudBackend.SendMessage(outgoing);
								}
							}
							else
							{
								// Route to local broker peer
								string peer = workers[0];
								workers.RemoveAt(0);

								using (var outgoing = new ZMessage())
								{
									outgoing.Add(new ZFrame(peer));
									outgoing.Add(new ZFrame());
									outgoing.Add(new ZFrame(incoming[2].ReadString()));

									localBackend.SendMessage(outgoing);
								}
							}
						}
					}

				}
			}
		}

		static Int16 Peering2_GetPort(string name) 
		{
			var hash = (Int16)(name.GetHashCode());
			if (hash < 1024)
			{
				hash += 1024;
			}
			return hash;
		}
	}
}