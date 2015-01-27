using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

using ZeroMQ;

namespace ZeroMQ.Test
{
	using FLCli;

	//
	// flcliapi class - Freelance Pattern agent class
	// Implements the Freelance Protocol at http://rfc.zeromq.org/spec:10
	//
	// Authors: Pieter Hintjens, Uli Riehm
	//

	// This API works in two halves, a common pattern for APIs that need to
	// run in the background. One half is an frontend object our application
	// creates and works with; the other half is a backend "agent" that runs
	// in a background thread. The frontend talks to the backend over an
	// inproc pipe socket:

	namespace FLCli
	{
		public class FLCliApi : IDisposable
		{
			// Our context wrapper
			ZContext context;

			ZActor actor;

			// Pipe through to flcliapi agent
			ZSocket pipe;

			public FLCliApi()
			{
				context = ZContext.Create();

				actor = ZActor.Create(context, this.Agent);
				actor.Start();

				pipe = actor.Frontend;
			}

			~FLCliApi() 
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
					if (context != null)
					{
						context.Dispose();
						context = null;
					}
				}
			}

			public void Connect(string endpoint)
			{
				// To implement the connect method, the frontend object sends a multipart
				// message to the backend agent. The first part is a string "CONNECT", and
				// the second part is the endpoint. It waits 100msec for the connection to
				// come up, which isn't pretty, but saves us from sending all requests to a
				// single server, at startup time:

				using (var message = new ZMessage())
				{
					message.Add(new ZFrame("CONNECT"));
					message.Add(new ZFrame(endpoint));

					pipe.Send(message);
				}

				Thread.Sleep(100);	// Allow connection to come up
			}

			public ZMessage Request(ZMessage request)
			{
				// To implement the request method, the frontend object sends a message
				// to the backend, specifying a command "REQUEST" and the request message:

				request.Prepend(new ZFrame("REQUEST"));
				pipe.Send(request);

				ZMessage reply = pipe.ReceiveMessage();

				using (ZFrame statusFrame = reply.Pop())
				{
					string status = statusFrame.ReadString();
					if (status == "FAILED")
					{
						reply.Dispose();
						reply = null;
					}
				}

				return reply;
			}

			public void Agent(CancellationToken cancellus, object[] args, ZSocket backend)
			{
				// Finally, here's the agent task itself, which polls its two sockets
				// and processes incoming messages:

				using (var agent = new Agent(context, backend))
				{
					var p = ZPollItem.CreateReceiver();

					while (!cancellus.IsCancellationRequested)
					{
						// Calculate tickless timer, up to 1 hour
						var tickless = DateTime.UtcNow + TimeSpan.FromMinutes(1);

						if (agent.request != null && tickless > agent.expires)
							tickless = agent.expires;

						foreach (Server server in agent.servers)
							server.Tickless(ref tickless);

						ZError error;
						ZMessage msg;

						if (agent.pipe.PollIn(p, out msg, out error, tickless - DateTime.UtcNow))
						{
							using (msg)
							{
								agent.ControlMessage(msg);
							}
						}
						else
						{
							if (error == ZError.ETERM)
								break;	// Interrupted
							if (error != ZError.EAGAIN)
								throw new ZException(error);
						}

						if (agent.router.PollIn(p, out msg, out error, tickless - DateTime.UtcNow))
						{
							using (msg)
							{
								agent.RouterMessage(msg);
							}
						}
						else
						{
							if (error == ZError.ETERM)
								break;	// Interrupted
							if (error != ZError.EAGAIN)
								throw new ZException(error);
						}

						if (agent.request != null)
						{
							// If we're processing a request, dispatch to next server

							if (DateTime.UtcNow >= agent.expires)
							{
								// Request expired, kill it
								using (var outgoing = new ZFrame("FAILED"))
								{
									agent.pipe.Send(outgoing);
								}

								agent.request.Dispose();
								agent.request = null;
							}
							else
							{
								// Find server to talk to, remove any expired ones
								foreach (Server server in agent.actives.ToList())
								{
									if (DateTime.UtcNow >= server.expires)
									{
										agent.actives.Remove(server);
										server.alive = false;
									}
									else
									{
										using (var request = agent.request.Duplicate())
										{
											request.Prepend(new ZFrame(server.endpoint));

											agent.router.Send(request);
											break;
										}
									}
								}
							}
						}

						// Disconnect and delete any expired servers
						// Send heartbeats to idle servers if needed
						foreach (Server server in agent.servers)
						{
							server.Ping(agent.router);
						}
					}
				}
			}
		}

		public class Agent : IDisposable
		{
			static readonly TimeSpan GLOBAL_TIMEOUT = TimeSpan.FromMilliseconds(3000);

			// We build the agent as a class that's capable of processing messages
			// coming in from its various sockets:

			// Simple class for one background agent

			// Own context
			ZContext context;

			// Socket to talk back to application
			public ZSocket pipe;

			// Socket to talk to servers
			public ZSocket router;

			// Servers we've connected to
			public HashSet<Server> servers;

			// Servers we know are alive
			public List<Server> actives;

			// Number of requests ever sent
			int sequence;

			// Current request if any
			public ZMessage request;

			// Current reply if any
			ZMessage reply;

			// Timeout for request/reply
			public DateTime expires;

			public Agent(ZContext context, ZSocket pipe)
			{
				this.context = context;
				this.pipe = pipe;

				this.router = ZSocket.Create(context, ZSocketType.ROUTER);
				this.servers = new HashSet<Server>();
				this.actives = new List<Server>();
			}

			~Agent() 
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
					this.servers = null;
					this.actives = null;

					if (request != null)
					{
						request.Dispose();
						request = null;
					}
					if (reply != null)
					{
						reply.Dispose();
						reply = null;
					}
					if (context != null)
					{
						context.Dispose();
						context = null;
					}
				}
			}

			public void ControlMessage(ZMessage msg)
			{
				// This method processes one message from our frontend class
				// (it's going to be CONNECT or REQUEST):

				string command = msg[0].ReadString();

				if (command == "CONNECT")
				{
					string endpoint = msg[1].ReadString();
					Console.WriteLine("I: connecting to {0}...", endpoint);

					router.Connect(endpoint);

					var server = new Server(endpoint);
					servers.Add(server);
					actives.Add(server);
				}
				else if (command == "REQUEST")
				{
					if (request != null)
					{
						// Strict request-reply cycle
						throw new InvalidOperationException();
					}

					// Prefix request with sequence number and empty envelope
					msg.Prepend(new ZFrame(++sequence));

					// Take ownership of request message
					request = msg.Duplicate();

					// Request expires after global timeout
					expires = DateTime.UtcNow + GLOBAL_TIMEOUT;
				}
			}

			public void RouterMessage(ZMessage reply)
			{
				// This method processes one message from a connected
				// server:

				// Frame 0 is server that replied
				string endpoint = reply[0].ReadString();

				Server server = servers.Single(s => s.endpoint == endpoint);
				if (!server.alive)
				{
					actives.Add(server);
					server.alive = true;
				}
				server.pingAt = DateTime.UtcNow + Server.PING_INTERVAL;
				server.expires = DateTime.UtcNow + Server.SERVER_TTL;

				// Frame 1 may be sequence number for reply
				int sequence = reply[1].ReadInt32();
				if (sequence == this.sequence)
				{
					reply.RemoveAt(0);
					reply.RemoveAt(0);
					reply.Prepend(new ZFrame("OK"));

					pipe.Send(reply);

					request.Dispose();
					request = null;
				}
			}
		}

		public class Server 
		{
			public static readonly TimeSpan PING_INTERVAL = TimeSpan.FromMilliseconds(2000);

			public static readonly TimeSpan SERVER_TTL = TimeSpan.FromMilliseconds(6000);

			// Here we see the backend agent. It runs as an attached thread, talking
			// to its parent over a pipe socket. It is a fairly complex piece of work
			// so we'll break it down into pieces. First, the agent manages a set of
			// servers, using our familiar class approach:

			// Simple class for one server we talk to

			// Server identity/endpoint
			public string endpoint;

			// 1 if known to be alive
			public bool alive;

			// Next ping at this time
			public DateTime pingAt;

			// Expires at this time
			public DateTime expires;

			public Server(string endpoint)
			{
				this.endpoint = endpoint;
				// this.alive = false;
				this.pingAt = DateTime.UtcNow + PING_INTERVAL;
				this.expires = DateTime.UtcNow + SERVER_TTL;
			}

			public void Ping(ZSocket socket)
			{
				if (DateTime.UtcNow >= pingAt)
				{
					using (var outgoing = new ZMessage())
					{
						outgoing.Add(new ZFrame(endpoint));
						outgoing.Add(new ZFrame("PING"));

						socket.Send(outgoing);
					}

					pingAt = DateTime.UtcNow + PING_INTERVAL;
				}
			}

			public void Tickless(ref DateTime tickless)
			{
				if (tickless > pingAt)
					tickless = pingAt;
			}
		}
	}
}