using System;
using System.Collections.Generic;
using System.Threading;

using ZeroMQ;

namespace Examples
{
	public partial class Program
	{
		public static void Beispiel(IDictionary<string, string> dict, string[] args)
		{
			//
			// Simple REQ <=> REP
			//
			// Author: metadings
			//

			if (args == null || args.Length < 1)
			{
				args = new string[] { "World", "You" };
			}

			// Setup the ZContext
			using (var ctx = new ZContext())
			{
				// Create a cancellor
				var cancellor = new CancellationTokenSource();

				// Start the "Server"
				new Thread( () => Server(ctx, cancellor.Token) ).Start();

				// Now we are the Client, asking the Server
				foreach (string arg in args)
				{
					Console.WriteLine( Client(ctx, arg) );
				}

				// Shutdown the Server
				// ctx.Shutdown();
				// Thread.Sleep(1);

				// Cancel the Server
				cancellor.Cancel();
			}
		}

		static void Server(ZContext ctx, CancellationToken cancellus)
		{
			using (var socket = new ZSocket(ctx, ZSocketType.REP))
			{
				socket.Bind("inproc://helloworld");

				ZFrame request;
				ZError error;

				while (!cancellus.IsCancellationRequested)
				{
					if (null == (request = socket.ReceiveFrame(ZSocketFlags.DontWait, out error)))
					{
						if (error == ZError.EAGAIN)
						{
							Thread.Sleep(1);
							continue;
						}
						if (error == ZError.ETERM)
							break;  // Interrupted
						throw new ZException(error);
					}

					using (request)
					{
						// Let the response be "Hello " + input
						socket.Send(new ZFrame("Hello " + request.ReadString()));
					}
				}
			}
		}

		static string Client(ZContext ctx, string name)
		{
			using (var socket = new ZSocket(ctx, ZSocketType.REQ))
			{
				socket.Connect("inproc://helloworld");

				socket.Send(new ZFrame(name));

				using (ZFrame response = socket.ReceiveFrame())
				{
					return response.ReadString();
				}
			}
		}

	}
}

