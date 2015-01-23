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
		static void AsyncSrv_Client(ZContext context, int i) 
		{
			//
			// Asynchronous client-to-server (DEALER to ROUTER)
			//
			// While this example runs in a single process, that is to make
			// it easier to start and stop the example. Each task has its own
			// context and conceptually acts as a separate process.
			//
			// Authors: Pieter Hintjens, Uli Riehm
			//

			// This is our client task
			// It connects to the server, and then sends a request once per second
			// It collects responses as they arrive, and it prints them out. We will
			// run several client tasks in parallel, each with a different random ID.

			using (var client = ZSocket.Create(context, ZSocketType.DEALER))
			{
				// Set identity to make tracing easier
				client.Identity = Encoding.UTF8.GetBytes("CLIENT" + i);
				// Connect
				client.Connect("tcp://localhost:5570");

				var poll = ZPollItem.CreateReceiver();

				int requests = 0;
				ZError error;
				ZMessage incoming;

				while (true)
				{
					// Tick once per second, pulling in arriving messages
					for (int centitick = 0; centitick < 100; ++centitick)
					{
						if (!client.PollIn(poll, out incoming, out error, TimeSpan.FromMilliseconds(10)))
						{
							if (error == ZError.EAGAIN)
							{
								error = ZError.None;
								continue;
							}
							if (error == ZError.ETERM)
							{
								return;
							}

							throw new ZException(error);
						}
						using (incoming)
						{
							string messageText = incoming[0].ReadString();
							Console.WriteLine("[CLIENT{0}] {1}", i, messageText);
						}
					}
					using (var outgoing = new ZMessage())
					{
						outgoing.Add(new ZFrame(client.Identity, 0, client.Identity.Length));
						outgoing.Add(new ZFrame());
						outgoing.Add(new ZFrame("request " + (++requests)));

						if (!client.Send(outgoing, out error))
						{
							if (error == ZError.ETERM)
							{
								return;
							}
							throw new ZException(error);
						}
					}
				}
			}
		}

		// This is our server task.
		// It uses the multithreaded server model to deal requests out to a pool
		// of workers and route replies back to clients. One worker can handle
		// one request at a time but one client can talk to multiple workers at
		// once.
		static void AsyncSrv_ServerTask(ZContext context) 
		{
			using (var frontend = ZSocket.Create(context, ZSocketType.ROUTER))
			using (var backend = ZSocket.Create(context, ZSocketType.DEALER))
			{
				// Frontend socket talks to clients over TCP
				frontend.Bind("tcp://*:5570");
				// Backend socket talks to workers over inproc
				backend.Bind("inproc://backend");

				// Launch pool of worker threads, precise number is not critical
				for (int i = 0; i < 5; ++i)
				{
					int j = i; new Thread(() => AsyncSrv_ServerWorker(context, j)).Start();
				}

				ZError error;

				// Connect backend to frontend via a proxy
				if (!ZContext.Proxy(frontend, backend, out error))
				{
					if (error == ZError.ETERM)
						return;

					throw new ZException(error);
				}
			}
		}

		// Each worker task works on one request at a time and sends a random number
		// of replies back, with random delays between replies:
		static void AsyncSrv_ServerWorker(ZContext context, int i) 
		{
			using (var worker = ZSocket.Create(context, ZSocketType.DEALER))
			{
				worker.Connect("inproc://backend");

				ZError error;
				var rnd = new Random();

				while (true)
				{
					ZMessage request;
					if (null == (request = worker.ReceiveMessage(out error)))
					{
						if (error == ZError.ETERM)
							return;

						throw new ZException(error);
					}

					using (request)
					{
						// The DEALER socket gives us the reply envelope and message
						string identity = request[0].ReadString();
						string content = request[1].ReadString();

						// Send 0..4 replies back
						int replies = rnd.Next(5);
						for (int reply = 0; reply < replies; ++reply)
						{
							// Sleep for some fraction of a second
							Thread.Sleep(rnd.Next(1000) + 1);

							using (var response = new ZMessage())
							{
								response.Add(new ZFrame(identity));
								response.Add(new ZFrame(content));

								if (!worker.Send(response, out error))
								{
									if (error == ZError.ETERM)
										return;

									throw new ZException(error);
								}
							}
						}
					}
				}
			}
		}

		// The main thread simply starts several clients and a server, and then
		// waits for the server to finish.
		public static void AsyncSrv(IDictionary<string, string> dict, string[] args)
		{
			using (var context = ZContext.Create())
			{
				for (int i = 0; i < 5; ++i)
				{
					int j = i; new Thread(() => AsyncSrv_Client(context, j)).Start();
				}
				new Thread(() => AsyncSrv_ServerTask(context)).Start();

				// Run for 5 seconds then quit
				Thread.Sleep(5 * 1000);
			}
		}
	}
}