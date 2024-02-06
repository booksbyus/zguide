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
		public static void RRBroker(string[] args)
		{
			//
			// Simple request-reply broker
			//
			// Author: metadings
			//

			// Prepare our context and sockets
			using (var ctx = new ZContext())
			using (var frontend = new ZSocket(ctx, ZSocketType.ROUTER))
			using (var backend = new ZSocket(ctx, ZSocketType.DEALER))
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
						Console_WriteZMessage("frontend", 2, message);
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
						Console_WriteZMessage(" backend", 2, message);
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