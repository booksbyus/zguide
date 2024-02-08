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
		public static void SPQueue(string[] args)
		{
			//
			// Simple Pirate broker
			// This is identical to load-balancing pattern, with no reliability
			// mechanisms. It depends on the client for recovery. Runs forever.
			//
			// Author: metadings
			//

			using (var context = new ZContext())
			using (var frontend = new ZSocket(context, ZSocketType.ROUTER))
			using (var backend = new ZSocket(context, ZSocketType.ROUTER))
			{
				frontend.Bind("tcp://*:5555");
				backend.Bind("tcp://*:5556");

				// Queue of available workers
				var worker_queue = new List<string>();

				ZError error;
				ZMessage incoming;
				var poll = ZPollItem.CreateReceiver();

				while (true)
				{
					if (backend.PollIn(poll, out incoming, out error, TimeSpan.FromMilliseconds(64)))
					{
						using (incoming)
						{
							// Handle worker activity on backend

							// incoming[0] is worker_id
							string worker_id = incoming[0].ReadString();
							// Queue worker identity for load-balancing
							worker_queue.Add(worker_id);

							// incoming[1] is empty

							// incoming[2] is READY or else client_id
							string client_id = incoming[2].ReadString();

							if (client_id == "READY")
							{
								Console.WriteLine("I: ({0}) worker ready", worker_id);
							}
							else
							{
								// incoming[3] is empty

								// incoming[4] is reply
								// string reply = incoming[4].ReadString();
								// int reply = incoming[4].ReadInt32();

								Console.WriteLine("I: ({0}) work complete", worker_id);

								using (var outgoing = new ZMessage())
								{
									outgoing.Add(new ZFrame(client_id));
									outgoing.Add(new ZFrame());
									outgoing.Add(incoming[4]);

									// Send
									frontend.Send(outgoing);
								}
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

					if (worker_queue.Count > 0)
					{
						// Poll frontend only if we have available workers

						if (frontend.PollIn(poll, out incoming, out error, TimeSpan.FromMilliseconds(64)))
						{
							using (incoming)
							{
								// Here is how we handle a client request

								// Dequeue the next worker identity
								string worker_id = worker_queue[0];
								worker_queue.RemoveAt(0);

								// incoming[0] is client_id
								string client_id = incoming[0].ReadString();

								// incoming[1] is empty

								// incoming[2] is request
								// string request = incoming[2].ReadString();
								int request = incoming[2].ReadInt32();

								Console.WriteLine("I: ({0}) working on ({1}) {2}", worker_id, client_id, request);

								using (var outgoing = new ZMessage())
								{
									outgoing.Add(new ZFrame(worker_id));
									outgoing.Add(new ZFrame());
									outgoing.Add(new ZFrame(client_id));
									outgoing.Add(new ZFrame());
									outgoing.Add(incoming[2]);

									// Send
									backend.Send(outgoing);
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
					}
				}
			}
		}
	}
}