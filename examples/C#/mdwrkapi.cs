using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading;

using ZeroMQ;

namespace Examples
{
	namespace MDWrkApi
	{
		//
		//  mdwrkapi class - Majordomo Protocol Worker API
		//  Implements the MDP/Worker spec at http://rfc.zeromq.org/spec:7.
		//
		// Author: chubbson
		//

		public class MajordomoWorker : IDisposable
		{
			//  Structure of our class
			//  We access these properties only via class methods

			// Our context
			private ZContext _context;

			// Majordomo broker
			public string Broker { get; protected set; }
			public string Service { get; protected set; }

			//  Socket to broker
			public ZSocket Worker { get; protected set; }

			//  Print activity to console
			public bool Verbose { get; protected set; }


			#region Heartbeat management

			// When to send HEARTBEAT
			public DateTime HeartbeatAt { get; protected set; }

			// How many attempts left
			public UInt64 Liveness { get; protected set; }

			// Heartbeat delay, msecs
			public TimeSpan Heartbeat { get; protected set; }

			// Reconnect delay, msecs
			public TimeSpan Reconnect { get; protected set; }

			#endregion

			#region Unknown to port

			// Zero only at start
			private bool _expectReply;

			// Return identity, if any
			private ZFrame _replyTo;

			#endregion


			public void SendToBroker(string command, string option, ZMessage msg)
			{
				using (msg = msg != null ? msg.Duplicate() : new ZMessage())
				{
					if (!string.IsNullOrEmpty(option))
						msg.Prepend(new ZFrame(option));
					msg.Prepend(new ZFrame(command));
					msg.Prepend(new ZFrame(MdpCommon.MDPW_WORKER));
					msg.Prepend(new ZFrame(string.Empty));

					if (Verbose)
						msg.DumpZmsg("I: sending '{0:X}|{0}' to broker", command.ToMdCmd());

					Worker.Send(msg);
				}
			}

			public void ConnectToBroker()
			{
				//  Connect or reconnect to broker
				Worker = new ZSocket(_context, ZSocketType.DEALER);
				Worker.Connect(Broker);
				if (Verbose)
					"I: connecting to broker at {0}...".DumpString(Broker);

				// Register service with broker
				SendToBroker(MdpCommon.MdpwCmd.READY.ToHexString(), Service, null);

				// if liveness hits zero, queue is considered disconnected
				Liveness = MdpCommon.HEARTBEAT_LIVENESS; 
				HeartbeatAt = DateTime.UtcNow + Heartbeat;
			}

			public MajordomoWorker(string broker, string service, bool verbose)
			{
				if(broker == null)
					throw new InvalidOperationException();
				if(service == null)
					throw new InvalidOperationException();

				_context = new ZContext();
				Broker = broker;
				Service = service;
				Verbose = verbose;
				Heartbeat = MdpCommon.HEARTBEAT_DELAY;
				Reconnect = MdpCommon.RECONNECT_DELAY;

				ConnectToBroker();
			}

			~MajordomoWorker()
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

					if (Worker != null)
					{
						Worker.Dispose();
						Worker = null;
					}
					//Do not Dispose Context: cuz of weird shutdown behavior, stucks in using calls
				}
			}

			//  .split configure worker
			//  We provide two methods to configure the worker API. You can set the
			//  heartbeat interval and retries to match the expected network performance.

			//  Set heartbeat delay
			public void Set_Heartbeat(int heartbeatInMs)
			{
				Heartbeat = TimeSpan.FromMilliseconds(heartbeatInMs);
			}

			//  Set reconnect delay
			public void Set_Reconnect(int reconnectInMs)
			{
				Reconnect = TimeSpan.FromMilliseconds(reconnectInMs);
			}


			//  .split recv method
			//  This is the {{recv}} method; it's a little misnamed because it first sends
			//  any reply and then waits for a new request. If you have a better name
			//  for this, let me know.

			//  Send reply, if any, to broker and wait for next request.
			public ZMessage Recv(ZMessage reply, CancellationTokenSource cancellor)
			{
				if (reply == null
					&& _expectReply)
					throw new InvalidOperationException();

				if (reply != null)
				{
					if(_replyTo == null)
						throw new InvalidOperationException();
					reply.Wrap(_replyTo);
					SendToBroker(MdpCommon.MdpwCmd.REPLY.ToHexString(), string.Empty, reply);
				}
				_expectReply = true;

				while (true)
				{
					if (cancellor.IsCancellationRequested
						|| (Console.KeyAvailable && Console.ReadKey(true).Key == ConsoleKey.Escape))
						_context.Shutdown();

					var p = ZPollItem.CreateReceiver();
					ZMessage msg;
					ZError error;
					if (Worker.PollIn(p, out msg, out error, Heartbeat))
					{
						using (msg)
						{
							// If we got a reply
							if (Verbose)
								msg.DumpZmsg("I: received message from broker:");

							Liveness = MdpCommon.HEARTBEAT_LIVENESS;

							// Don't try to handle errors, just assert noisily
							if(msg.Count < 3)
								throw new InvalidOperationException();

							using (ZFrame empty = msg.Pop())
							{
								if (!empty.ToString().Equals(""))
									throw new InvalidOperationException();
							}

							using (ZFrame header = msg.Pop())
							{
								if (!header.ToString().Equals(MdpCommon.MDPW_WORKER))
									throw new InvalidOperationException();
							}
							//header.ReadString().Equals(MDPW_WORKER);
							using (ZFrame command = msg.Pop())
							{
								if (command.StrHexEq(MdpCommon.MdpwCmd.REQUEST))
								{
									//  We should pop and save as many addresses as there are
									//  up to a null part, but for now, just save one...
									_replyTo = msg.Unwrap();

									//  .split process message
									//  Here is where we actually have a message to process; we
									//  return it to the caller application:
									return msg.Duplicate();
								}
								else if (command.StrHexEq(MdpCommon.MdpwCmd.HEARTBEAT))
								{
									// Do nothing for heartbeats
								}
								else if (command.StrHexEq(MdpCommon.MdpwCmd.DISCONNECT))
								{
									ConnectToBroker();
								}
								else
									"E: invalid input message: '{0}'".DumpString(command.ToString());
							}
						}
					}
					else if (Equals(error, ZError.ETERM))
					{
						cancellor.Cancel();
						break; // Interrupted
					}
					else if (Equals(error, ZError.EAGAIN) 
							 && --Liveness == 0)
					{
						if (Verbose)
							"W: disconnected from broker - retrying...".DumpString();
						Thread.Sleep(Reconnect);
						ConnectToBroker();
					}

					// Send HEARTBEAT if it's time
					if (DateTime.UtcNow > HeartbeatAt)
					{
						SendToBroker(MdpCommon.MdpwCmd.HEARTBEAT.ToHexString(), null, null);
						HeartbeatAt = DateTime.UtcNow + Heartbeat;
					}
				}
				if (cancellor.IsCancellationRequested)
					"W: interrupt received, killing worker...\n".DumpString();

				return null;
			}
		}
	}
}
