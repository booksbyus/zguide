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
		// Broker peering simulation (part 2)
		// Prototypes the request-reply flow
		//
		// Author: metadings
		//

		static int Peering2_Clients = 10;
		static int Peering2_Workers = 3;

		static void Peering2_ClientTask(ZContext context, int i, string name, string message) 
		{
			// The client task does a request-reply dialog
			// using a standard synchronous REQ socket

			using (var client = new ZSocket(context, ZSocketType.REQ))
			{
				// Set printable identity
				client.IdentityString = name;

				// Connect
				client.Connect("tcp://127.0.0.1:" + Peering2_GetPort(name) + 1);

				ZError error;

				while (true)
				{
					// Send
					using (var outgoing = new ZFrame(message))
					{
						client.Send(outgoing);
					}

					// Receive
					ZFrame incoming = client.ReceiveFrame(out error);

					if (incoming == null)
					{
						if (error == ZError.ETERM)
							return;	// Interrupted

						throw new ZException(error);
					}
					using (incoming)
					{
						Console.WriteLine("Client {0}: {1}", name, incoming.ReadString());
					}
				}
			}
		}

		static void Peering2_WorkerTask(ZContext context, int i, string name) 
		{
			// The worker task plugs into the load-balancer using a REQ socket

			using (var worker = new ZSocket(context, ZSocketType.REQ))
			{
				// Set printable identity
				worker.IdentityString = name;

				// Connect
				worker.Connect("tcp://127.0.0.1:" + Peering2_GetPort(name) + 2);

				// Tell broker we're ready for work
				worker.Send(new ZFrame("READY"));

				// Process messages as they arrive
				ZError error;
				while (true)
				{
					// Receive
					ZFrame incoming = worker.ReceiveFrame(out error);

					if (incoming == null)
					{
						if (error == ZError.ETERM)
							return;	// Interrupted

						throw new ZException(error);
					}
					using (incoming)
					{
						Console.WriteLine("Worker {0}: {1}", name, incoming.ReadString());

						// Do some heavy work
						Thread.Sleep(1);

						// Send
						using (var outgoing = new ZFrame("OK"))
						{
							worker.Send(outgoing);
						}
					}
				}
			}
		}

		// The main task begins by setting-up its frontend and backend sockets
		// and then starting its client and worker tasks:
		public static void Peering2(string[] args)
		{
			// First argument is this broker's name
			// Other arguments are our peers' names
			//
			if (args == null || args.Length < 3)
			{
				if (args != null && args.Length == 1)
				{
					args = new string[] { args[0], "Me", "You" };
				}
				else
				{
					Console.WriteLine("Usage: {0} Peering2 Hello Me You", AppDomain.CurrentDomain.FriendlyName);
					Console.WriteLine("       {0} Peering2 Message You Me", AppDomain.CurrentDomain.FriendlyName);
					return;
				}
			}

			string message = args[0];

			string name = args[1];
			Console.WriteLine("I: preparing broker as {0}", name);

			using (var context = new ZContext())
			using (var cloudFrontend = new ZSocket(context, ZSocketType.ROUTER))
			using (var cloudBackend = new ZSocket(context, ZSocketType.ROUTER))
			using (var localFrontend = new ZSocket(context, ZSocketType.ROUTER))
			using (var localBackend = new ZSocket(context, ZSocketType.ROUTER))
			{
				// Bind cloud frontend to endpoint
				cloudFrontend.IdentityString = name;
				cloudFrontend.Bind("tcp://127.0.0.1:" + Peering2_GetPort(name) + 0);

				// Connect cloud backend to all peers
				cloudBackend.IdentityString = name;
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

				ZError error;
				ZMessage incoming;
				TimeSpan? wait;
				var poll = ZPollItem.CreateReceiver();

				while (true)
				{
					// If we have no workers, wait indefinitely
					wait = workers.Count > 0 ? (TimeSpan?)TimeSpan.FromMilliseconds(1000) : null;

					// Poll localBackend
					if (localBackend.PollIn(poll, out incoming, out error, wait))
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
					else if (error == ZError.EAGAIN && cloudBackend.PollIn(poll, out incoming, out error, wait))
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
									cloudFrontend.Send(incoming);

								incoming = null;
								break;
							}
						}
					}

					// Route reply to client if we still need to
					if (incoming != null)
					{
						using (incoming)
							localFrontend.Send(incoming);

						incoming = null;
					}

					// Now we route as many client requests as we have worker capacity
					// for. We may reroute requests from our local frontend, but not from //
					// the cloud frontend. We reroute randomly now, just to test things
					// out. In the next version, we'll do this properly by calculating
					// cloud capacity://

					var rnd = new Random();

					while (workers.Count > 0)
					{
						int reroutable = 0;

						// We'll do peer brokers first, to prevent starvation

						if (localFrontend.PollIn(poll, out incoming, out error, TimeSpan.FromMilliseconds(64)))
						{
							reroutable = 0;
						}
						else if (error == ZError.EAGAIN && cloudFrontend.PollIn(poll, out incoming, out error, TimeSpan.FromMilliseconds(64)))
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
							// If reroutable, send to cloud 25% of the time
							// Here we'd normally use cloud status information
							//
							if (reroutable == 1 && rnd.Next(4) == 0)
							{
								// Route to random broker peer

								int peer = rnd.Next(args.Length - 2) + 2;

								incoming.ReplaceAt(0, new ZFrame(args[peer]));

								/* using (var outgoing = new ZMessage())
								{
									outgoing.Add(new ZFrame(args[peer]));
									outgoing.Add(new ZFrame());
									outgoing.Add(incoming[2]);

									cloudBackend.Send(outgoing);
								} /**/

								cloudBackend.Send(incoming);
							}
							else
							{
								// Route to local broker peer

								string peer = workers[0];

								workers.RemoveAt(0);
								incoming.ReplaceAt(0, new ZFrame(peer));

								/* using (var outgoing = new ZMessage())
								{
									outgoing.Add(new ZFrame(peer));
									outgoing.Add(new ZFrame());
									outgoing.Add(incoming[2]);

									localBackend.Send(outgoing);
								} /**/

								localBackend.Send(incoming);
							}
						}
					}
				}
			}
		}

		static Int16 Peering2_GetPort(string name) 
		{
			var hash = (Int16)name.GetHashCode();
			if (hash < 1024)
			{
				hash += 1024;
			}
			return hash;
		}
	}
}