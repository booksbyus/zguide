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
		public static void RRBroker(IDictionary<string, string> dict, string[] args)
		{
			//
			// Simple request-reply broker
			//
			// Authors: Pieter Hintjens, Uli Riehm
			//

			// Prepare our context and sockets
			using (var context = ZContext.Create())
			using (var frontend = ZSocket.Create(context, ZSocketType.ROUTER))
			using (var backend = ZSocket.Create(context, ZSocketType.DEALER))
			{
				frontend.Bind("tcp://*:5559");
				backend.Bind("tcp://*:5560");

				// Initialize poll set
				var poll = ZPollItem.CreateReceiver();

				// Switch messages between sockets
				ZError error;
				ZMessage message;
				while (true)
				{
					if (frontend.PollIn(poll, out message, out error, TimeSpan.FromMilliseconds(64)))
					{
						// Process all parts of the message
						backend.Send(message);
					}
					else
					{
						if (error == ZError.ETERM)
							return;	// Interrupted
						if (error != ZError.EAGAIN)
							throw new ZException(error);
					}

					if (backend.PollIn(poll, out message, out error, TimeSpan.FromMilliseconds(64)))
					{
						// Process all parts of the message
						frontend.Send(message);
					}
					else
					{
						if (error == ZError.ETERM)
							return;	// Interrupted
						if (error != ZError.EAGAIN)
							throw new ZException(error);
					}
				}
			}
		}
	}
}