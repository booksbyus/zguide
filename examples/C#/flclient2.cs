using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

using ZeroMQ;

namespace Examples
{
	using FLClient2;

	//
	// Freelance client - Model 2
	// Uses DEALER socket to blast one or more services
	//
	// Author: metadings
	//

	namespace FLClient2
	{
		public class FLClient : IDisposable
		{
			// If not a single service replies within this time, give up
			static readonly TimeSpan GLOBAL_TIMEOUT = TimeSpan.FromMilliseconds(2500);

			// Here is the flclient class implementation. Each instance has a
			// context, a DEALER socket it uses to talk to the servers, a counter
			// of how many servers it's connected to, and a request sequence number:

			// Our context wrapper
			ZContext context;

			// DEALER socket talking to servers
			ZSocket socket;

			// How many servers we have connected to
			int servers;

			// Number of requests ever sent
			int sequence;

			public FLClient() 
			{
				// Constructor

				context = new ZContext();
				socket = new ZSocket(context, ZSocketType.DEALER);
				socket.Linger = GLOBAL_TIMEOUT;
			}

			~FLClient() 
			{
				Dispose(false);
			}

			public void Dispose()
			{
				GC.SuppressFinalize(this);
				Dispose(true);
			}

			protected void Dispose(bool disposing)
			{
				if (disposing)
				{
					// Destructor

					if (socket != null)
					{
						socket.Dispose();
						socket = null;
					}
					if (context != null)
					{
						context.Dispose();
						context = null;
					}
				}
			}

			public void Connect(string endpoint) 
			{
				// Connect to new server endpoint

				socket.Connect(endpoint);
				servers++;
			}

			public ZMessage Request(ZMessage request)
			{
				// This method does the hard work. It sends a request to all
				// connected servers in parallel (for this to work, all connections
				// must be successful and completed by this time). It then waits
				// for a single successful reply, and returns that to the caller.
				// Any other replies are just dropped:

				ZMessage reply = null;
				using (request)
				{
					// Prefix request with sequence number and empty envelope
					this.sequence++;
					request.Prepend(new ZFrame(this.sequence));
					request.Prepend(new ZFrame());

					// Blast the request to all connected servers
					for (int server = 0; server < this.servers; ++server)
					{
						using (var outgoing = request.Duplicate())
						{
							this.socket.Send(outgoing);
						}
					}

					// Wait for a matching reply to arrive from anywhere
					// Since we can poll several times, calculate each one
					ZError error;
					DateTime endtime = DateTime.UtcNow + GLOBAL_TIMEOUT;
					var poll = ZPollItem.CreateReceiver();
					while (endtime > DateTime.UtcNow)
					{
						if (this.socket.PollIn(poll, out reply, out error, endtime - DateTime.UtcNow))
						{
							// Reply is [empty][sequence][OK]
							if (reply.Count < 3)
							{
								throw new InvalidOperationException();
							}

							reply.RemoveAt(0);

							using (ZFrame sequenceFrame = reply.RemoveAt(0, false))
							{
								int sequence = sequenceFrame.ReadInt32();

								if (sequence == this.sequence)
								{
									break;	// Reply is ok
								}
							}

							reply.Dispose();
						}
						else
						{
							if (error == ZError.ETERM)
								break;	// Interrupted
							if (error != ZError.EAGAIN)
								throw new ZException(error);
						}
					}
				}
				return reply;
			}
		}
	}

	static partial class Program
	{
		public static void FLClient2(string[] args)
		{
			if (args == null || args.Length < 1)
			{
				Console.WriteLine();
				Console.WriteLine("Usage: ./{0} FLClient2 [Endpoint] ...", AppDomain.CurrentDomain.FriendlyName);
				Console.WriteLine();
				Console.WriteLine("    Endpoint  Where FLClient2 should connect to.");
				Console.WriteLine("              Default is tcp://127.0.0.1:7781");
				Console.WriteLine();
				args = new string[] { "tcp://127.0.0.1:7781" };
			}

			// Create new freelance client object
			using (var client = new FLClient())
			{
				// Connect to each endpoint
				for (int i = 0; i < args.Length; ++i)
				{
					client.Connect(args[i]);
				}

				// Send a bunch of name resolution 'requests', measure time
				int requests = 0;
				DateTime starttime = DateTime.UtcNow;
				var error = ZError.None;
				while (++requests < 10000)
				{
					var outgoing = new ZMessage();
					outgoing.Add(new ZFrame("random name"));

					ZMessage incoming = client.Request(outgoing);
					if (incoming == null)
					{
						error = ZError.ETERM;
						break;
					}
					incoming.Dispose(); // using (incoming) ;
				}

				if (error == ZError.ETERM)
					Console.WriteLine("E: name service not available, aborted.");

				else
					Console.WriteLine("Average round trip cost: {0} ms", (DateTime.UtcNow - starttime).TotalMilliseconds / requests);

			}
		}
	}
}