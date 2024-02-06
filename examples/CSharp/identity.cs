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
		public static void Identity(string[] args)
		{
			//
			// Demonstrate request-reply identities
			//
			// Author: metadings
			//

			using (var context = new ZContext())
			using (var sink = new ZSocket(context, ZSocketType.ROUTER))
			{
				sink.Bind("inproc://example");

				// First allow 0MQ to set the identity
				using (var anonymous = new ZSocket(context, ZSocketType.REQ))
				{
					anonymous.Connect("inproc://example");
					anonymous.Send(new ZFrame("ROUTER uses REQ's generated 5 byte identity"));
				}
				using (ZMessage msg = sink.ReceiveMessage())
				{    
					msg.DumpZmsg("--------------------------");
				}

				// Then set the identity ourselves
				using (var identified = new ZSocket(context, ZSocketType.REQ))
				{
					identified.IdentityString = "PEER2";
					identified.Connect("inproc://example");
					identified.Send(new ZFrame("ROUTER uses REQ's socket identity"));
				}
				using (ZMessage msg = sink.ReceiveMessage())
				{
					msg.DumpZmsg("--------------------------");
				}
			}
		}
	}
}