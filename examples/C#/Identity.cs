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
			using (var context = ZContext.Create())
			using (var sink = ZSocket.Create(context, ZSocketType.ROUTER))
			{
				sink.Bind("inproc://example");

				using (var anonymous = ZSocket.Create(context, ZSocketType.REQ))
				{
					anonymous.Connect("inproc://example");
					anonymous.SendFrame(ZFrame.From("ROUTER uses REQ's generated UUID"));
				}
				Identity_Dump(sink);

				using (var identified = ZSocket.Create(context, ZSocketType.REQ))
				{
					identified.Identity = Encoding.UTF8.GetBytes("PEER2");
					identified.Connect("inproc://example");
					identified.SendFrame(ZFrame.From("ROUTER uses REQ's socket identity"));
				}
				Identity_Dump(sink);
			}
		}

		static void Identity_Dump(ZSocket sink)
		{
			using (ZMessage msg = sink.ReceiveMessage())
			{
				Console.WriteLine("---");
				Console.WriteLine("[0] {0}", msg[0].ReadString());
				Console.WriteLine("[1] {0}", msg[1].ReadString());
				Console.WriteLine("[2] {0}", msg[2].ReadString());
			}
		}
	}
}