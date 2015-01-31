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
		public static void Identity(IDictionary<string, string> dict, string[] args)
		{
			//
			// Demonstrate request-reply identities
			//
			// Author: metadings (uli.riehm@metadea.de)
			//

			using (var context = ZContext.Create())
			using (var sink = ZSocket.Create(context, ZSocketType.ROUTER))
			{
				sink.Bind("inproc://example");

				// First allow 0MQ to set the identity
				using (var anonymous = ZSocket.Create(context, ZSocketType.REQ))
				{
					anonymous.Connect("inproc://example");
					anonymous.Send(new ZFrame("ROUTER uses REQ's generated UUID"));
				}
				using (ZMessage msg = sink.ReceiveMessage())
				{
					Console_WriteZMessage(msg, "---");
				}

				// Then set the identity ourselves
				using (var identified = ZSocket.Create(context, ZSocketType.REQ))
				{
					identified.IdentityString = "PEER2";
					identified.Connect("inproc://example");
					identified.Send(new ZFrame("ROUTER uses REQ's socket identity"));
				}
				using (ZMessage msg = sink.ReceiveMessage())
				{
					Console_WriteZMessage(msg, "---");
				}
			}
		}
	}
}