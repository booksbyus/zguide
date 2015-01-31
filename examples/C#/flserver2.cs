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
		public static void FLServer2(IDictionary<string, string> dict, string[] args)
		{
			//
			// Freelance server - Model 2
			// Does some work, replies OK, with message sequencing
			//
			// Author: metadings
			//

			if (args == null || args.Length < 1)
			{
				Console.WriteLine("I: syntax: {0} FLServer2 [endpoint]", AppDomain.CurrentDomain.FriendlyName);
				return;
			}

			string endpoint = args[0];

			using (var context = ZContext.Create())
			using (var server = ZSocket.Create(context, ZSocketType.REP))
			{
				server.Bind(endpoint);
				Console.WriteLine("I: server is ready as {0}", endpoint);

				ZError error;
				ZMessage incoming;

				while (true)
				{
					if (null == (incoming = server.ReceiveMessage(out error)))
					{
						if (error == ZError.ETERM)
							break;	// Interrupted
						throw new ZException(error);
					}

					ZFrame identity;
					using (incoming)
					{
						// Fail nastily if run against wrong client
						if (incoming.Count < 2)
						{
							throw new InvalidOperationException();
						}

						identity = incoming.RemoveAt(0, false);
					}
					using (identity)
					using (var outgoing = new ZMessage())
					{
						outgoing.Add(identity);
						outgoing.Add(new ZFrame("OK"));

						if (!server.Send(outgoing, out error))
						{
							if (error == ZError.ETERM)
								break;	// Interrupted
							throw new ZException(error);
						}
					}
				}

				if (error == ZError.ETERM)
				{
					Console.WriteLine("W: interrupted");
				}
			}
		}
	}
}