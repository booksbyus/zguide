using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

using ZeroMQ;

namespace Examples
{
	namespace FLCliApi
	{
		//
		// flcliapi class - Freelance Pattern agent class
		// Implements the Freelance Protocol at http://rfc.zeromq.org/spec:10
		//
		// Author: metadings
		//

		// This API works in two halves, a common pattern for APIs that need to
		// run in the background. One half is an frontend object our application
		// creates and works with; the other half is a backend "agent" that runs
		// in a background thread. The frontend talks to the backend over an
		// inproc pipe socket:

		public class FreelanceClient : IDisposable
		{
			// Here we see the backend agent. It runs as an attached thread, talking
			// to its parent over a pipe socket. It is a fairly complex piece of work
			// so we'll break it down into pieces.

			// Our context
			ZContext context;

			// Pipe through to flcliapi agent
			public ZActor Actor { get; protected set; }

			public FreelanceClient()
			{
				// Constructor
				this.context = new ZContext();

				this.Actor = new ZActor(this.context, FreelanceClient.Agent);
				this.Actor.Start();
			}

			~FreelanceClient() 
			{
				this.Dispose(false);
			}

			public void Dispose()
			{
				GC.SuppressFinalize(this);
				this.Dispose(true);
			}

			protected void Dispose(bool disposing)
			{
				if (disposing)
				{
					// Destructor

					if (this.Actor != null)
					{
						this.Actor.Dispose();
						this.Actor = null;
					}
					if (this.context != null)
					{
						// Do context.Dispose()

						this.context.Dispose();
						this.context = null;
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

					this.Actor.Frontend.Send(message);
				}

				Thread.Sleep(64);	// Allow connection to come up
			}

			public ZMessage Request(ZMessage request)
			{
				// To implement the request method, the frontend object sends a message
				// to the backend, specifying a command "REQUEST" and the request message:

				request.Prepend(new ZFrame("REQUEST"));

				this.Actor.Frontend.Send(request);

				ZMessage reply;
				ZError error;
				if (null != (reply = this.Actor.Frontend.ReceiveMessage(out error))) 
				{
					string status = reply.PopString();
					if (status == "FAILED")
					{
						reply.Dispose();
						reply = null;
					}
				}
				return reply;
			}

			public static void Agent(ZContext context, ZSocket backend, CancellationTokenSource cancellor, object[] args)
			{
				// Finally, here's the agent task itself, which polls its two sockets
				// and processes incoming messages:

				using (var agent = new Agent(context, backend))
				{
					var p = ZPollItem.CreateReceiver();

					while (!cancellor.IsCancellationRequested)
					{
						ZMessage msg;
						ZError error;

						// Poll the control message

						if (agent.Pipe.PollIn(p, out msg, out error, TimeSpan.FromMilliseconds(64)))
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

						// Poll the router message

						if (agent.Router.PollIn(p, out msg, out error, TimeSpan.FromMilliseconds(64)))
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

						if (agent.Request != null)
						{
							// If we're processing a request, dispatch to next server

							if (DateTime.UtcNow >= agent.Expires)
							{
								// Request expired, kill it
								using (var outgoing = new ZFrame("FAILED"))
								{
									agent.Pipe.Send(outgoing);
								}

								agent.Request.Dispose();
								agent.Request = null;
							}
							else
							{
								// Find server to talk to, remove any expired ones
								foreach (Server server in agent.Actives.ToList())
								{
									if (DateTime.UtcNow >= server.Expires)
									{
										agent.Actives.Remove(server);
										server.Alive = false;
									}
									else
									{
										// Copy the Request, Push the Endpoint and send on Router
										using (var request = agent.Request.Duplicate())
										{
											request.Prepend(new ZFrame(server.Endpoint));

											agent.Router.Send(request);
											break;
										}
									}
								}
							}
						}

						// Disconnect and delete any expired servers
						// Send heartbeats to idle servers if needed
						foreach (Server server in agent.Servers)
						{
							server.Ping(agent.Router);
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

			// Socket to talk back to application
			public ZSocket Pipe;

			// Socket to talk to servers
			public ZSocket Router;

			// Servers we've connected to
			public HashSet<Server> Servers;

			// Servers we know are alive
			public List<Server> Actives;

			// Number of requests ever sent
			int sequence;

			// Current request if any
			public ZMessage Request;

			// Current reply if any
			// ZMessage reply;

			// Timeout for request/reply
			public DateTime Expires;

			public Agent(ZContext context, ZSocket pipe)
				: this (context, default(string), pipe)
			{
				var rnd = new Random();
				this.Router.IdentityString = "CLIENT" + rnd.Next();
			}

			public Agent(ZContext context, string name, ZSocket pipe)
			{
				// Constructor
				this.Pipe = pipe;

				this.Router = new ZSocket(context, ZSocketType.ROUTER);
				if (name != null)
				{
					this.Router.IdentityString = name;
				}

				this.Servers = new HashSet<Server>();
				this.Actives = new List<Server>();
			}

			~Agent() 
			{
				this.Dispose(false);
			}

			public void Dispose()
			{
				GC.SuppressFinalize(this);
				this.Dispose(true);
			}

			protected void Dispose(bool disposing)
			{
				if (disposing)
				{
					// Destructor

					this.Servers = null;
					this.Actives = null;

					if (this.Request != null)
					{
						this.Request.Dispose();
						this.Request = null;
					}
					/* if (this.reply != null)
					{
						this.reply.Dispose();
						this.reply = null;
					} */

					if (this.Router != null)
					{
						this.Router.Dispose();
						this.Router = null;
					}
				}
			}

			public void ControlMessage(ZMessage msg)
			{
				// This method processes one message from our frontend class
				// (it's going to be CONNECT or REQUEST):

				string command = msg.PopString();

				if (command == "CONNECT")
				{
					string endpoint = msg.PopString();
					Console.WriteLine("I: connecting to {0}...", endpoint);

					this.Router.Connect(endpoint);

					var server = new Server(endpoint);
					this.Servers.Add(server);
					this.Actives.Add(server);
				}
				else if (command == "REQUEST")
				{
					if (this.Request != null)
					{
						// Strict request-reply cycle
						throw new InvalidOperationException();
					}

					// Prefix request with sequence number and empty envelope
					msg.Prepend(new ZFrame(++sequence));

					// Take ownership of request message
					this.Request = msg.Duplicate();

					// Request expires after global timeout
					this.Expires = DateTime.UtcNow + GLOBAL_TIMEOUT;
				}
			}

			public void RouterMessage(ZMessage reply)
			{
				// This method processes one message from a connected
				// server:

				// Frame 0 is server that replied
				string endpoint = reply.PopString();
				Server server = this.Servers.Single(s => s.Endpoint == endpoint);
				if (!server.Alive)
				{
					this.Actives.Add(server);
					server.Refresh(true);
				}

				// Frame 1 may be sequence number for reply
				int sequence = reply.PopInt32();
				if (sequence == this.sequence)
				{
					reply.Prepend(new ZFrame("OK"));

					this.Pipe.Send(reply);

					this.Request.Dispose();
					this.Request = null;
				}
			}
		}

		public class Server 
		{
			public static readonly TimeSpan PING_INTERVAL = TimeSpan.FromMilliseconds(2000);

			public static readonly TimeSpan SERVER_TTL = TimeSpan.FromMilliseconds(6000);

			// Simple class for one server we talk to

			// Server identity/endpoint
			public string Endpoint { get; protected set; }

			// true if known to be alive
			public bool Alive { get; set; }

			// Next ping at this time
			public DateTime PingAt { get; protected set; }

			// Expires at this time
			public DateTime Expires { get; protected set; }

			public Server(string endpoint)
			{
				this.Endpoint = endpoint;
				this.Refresh(true);
			}

			public void Refresh(bool alive)
			{
				this.Alive = alive;
				if (alive)
				{
					this.PingAt = DateTime.UtcNow + PING_INTERVAL;
					this.Expires = DateTime.UtcNow + SERVER_TTL;
				}
			}

			public void Ping(ZSocket socket)
			{
				if (DateTime.UtcNow >= PingAt)
				{
					using (var outgoing = new ZMessage())
					{
						outgoing.Add(new ZFrame(Endpoint));
						outgoing.Add(new ZFrame("PING"));

						socket.Send(outgoing);
					}

					this.PingAt = DateTime.UtcNow + PING_INTERVAL;
				}
			}

			public override int GetHashCode()
			{
				return Endpoint.GetHashCode();
			}
		}
	}
}