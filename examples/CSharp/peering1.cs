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
		// Broker peering simulation (part 1)
		// Prototypes the state flow
		//
		// Author: metadings
		//

		public static void Peering1(string[] args)
		{
			// First argument is this broker's name
			// Other arguments are our peers' names
			//
			if (args == null || args.Length < 2)
			{
				Console.WriteLine();
				Console.WriteLine("Usage: {0} Peering1 World Receiver0", AppDomain.CurrentDomain.FriendlyName);
				Console.WriteLine("       {0} Peering1 Receiver0 World", AppDomain.CurrentDomain.FriendlyName);
				Console.WriteLine();
				return;
			}
			string self = args[0];
			Console.WriteLine("I: preparing broker as {0}", self);

			using (var context = new ZContext())
			using (var backend = new ZSocket(context, ZSocketType.PUB))
			using (var frontend = new ZSocket(context, ZSocketType.SUB))
			{
				// Bind backend to endpoint
				backend.Bind("tcp://127.0.0.1:" + Peering1_GetPort(self));

				// Connect frontend to all peers
				frontend.SubscribeAll();
				for (int i = 1; i < args.Length; ++i)
				{
					string peer = args[i];
					Console.WriteLine("I: connecting to state backend at {0}", peer);
					frontend.Connect("tcp://127.0.0.1:" + Peering1_GetPort(peer));
				}

				// The main loop sends out status messages to peers, and collects
				// status messages back from peers. The zmq_poll timeout defines
				// our own heartbeat:

				ZError error;
				ZMessage incoming;
				var poll = ZPollItem.CreateReceiver();
				var rnd = new Random();

				while (true)
				{
					// Poll for activity, or 1 second timeout
					if (!frontend.PollIn(poll, out incoming, out error, TimeSpan.FromSeconds(1)))
					{
						if (error == ZError.EAGAIN)
						{
							using (var output = new ZMessage())
							{
								output.Add(new ZFrame(self));

								var outputNumber = ZFrame.Create(4);
								outputNumber.Write(rnd.Next(10));
								output.Add(outputNumber);

								backend.Send(output);
							}

							continue;
						}
						if (error == ZError.ETERM)
							return;

						throw new ZException(error);
					}
					using (incoming)
					{
						string peer_name = incoming[0].ReadString();
						int available = incoming[1].ReadInt32();
						Console.WriteLine("{0} - {1} workers free", peer_name, available);
					}
				}
			}
		}

		static Int16 Peering1_GetPort(string name)
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