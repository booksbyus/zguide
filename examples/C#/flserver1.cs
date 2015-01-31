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
		public static void FLServer1(IDictionary<string, string> dict, string[] args)
		{
			//
			// Freelance server - Model 1
			// Trivial echo service
			//
			// Author: metadings
			//

			if (args == null || args.Length < 1)
			{
				Console.WriteLine("I: syntax: {0} FLServer1 [endpoint]", AppDomain.CurrentDomain.FriendlyName);
				return;
			}

			using (var context = ZContext.Create())
			using (var server = ZSocket.Create(context, ZSocketType.REP)) 
			{
				server.Bind(args[0]);

				Console.WriteLine("I: echo service is ready at {0}", args[0]);

				ZMessage message;
				ZError error;
				while (true)
				{
					if (null != (message = server.ReceiveMessage(out error)))
					{
						using (message)
						{
							server.Send(message);
						}
					}
					else
					{
						if (error == ZError.ETERM)
							return;	// Interrupted
						throw new ZException(error);
					}
				}
			}
		}
	}
}