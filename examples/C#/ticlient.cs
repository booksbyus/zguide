using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.Remoting.Messaging;
using System.Text;
using System.Threading;
using ZeroMQ;

namespace Examples
{
	//  Titanic client example
	//  Implements client side of http://rfc.zeromq.org/spec:9

	//  Lets us build this source without creating a library
	using MDCliApi;

	static partial class Program
	{


		public static void TIClient(string[] args)
		{
			bool verbose = (args.Any(e => e.ToLower().Equals("-v")
										  || e.ToLower().Equals("--verbose")));
			Console.WriteLine("Verbose: {0}", verbose);

			CancellationTokenSource cancellor = new CancellationTokenSource();
			Console.CancelKeyPress += (s, ea) =>
			{
				ea.Cancel = true;
				cancellor.Cancel();
			};

			using (MajordomoClient session = new MajordomoClient("tcp://127.0.0.1:5555", verbose))
			{
				//  1. Send 'echo' request to Titanic
				ZMessage request = new ZMessage();
				request.Add(new ZFrame("echo"));
				request.Add(new ZFrame("Hello World"));

				Guid uuid = Guid.Empty; 
				using (var reply = TIClient_ServiceCall(session, "titanic.request", request, cancellor))
				{
					if (reply != null)
					{
						uuid = Guid.Parse(reply.PopString());
						"I: request UUID {0}".DumpString(uuid);
					}
				}

				// 2. Wait until we get a reply
				while (!cancellor.IsCancellationRequested)
				{
					Thread.Sleep(100);
					request.Dispose();
					request = new ZMessage();
					request.Add(new ZFrame(uuid.ToString()));
					var reply = TIClient_ServiceCall(session, "titanic.reply", request, cancellor);
					if (reply != null)
					{
						string replystring = reply.Last().ToString();
						"Reply: {0}\n".DumpString(replystring);
						reply.Dispose();

						// 3. Close Request
						request.Dispose();
						request = new ZMessage();
						request.Add(new ZFrame(uuid.ToString()));
						reply = TIClient_ServiceCall(session, "titanic.close", request, cancellor);
						reply.Dispose();
						break;
					}
					else
					{
						"I: no reply yet, trying again...\n".DumpString();
						Thread.Sleep(5000); // try again in 5 seconds
					}
				}
			}
		}

		//  Calls a TSP service
		//  Returns response if successful (status code 200 OK), else NULL
		static ZMessage TIClient_ServiceCall (MajordomoClient session, string service, ZMessage request, CancellationTokenSource cts)
		{
			using (var reply = session.Send(service, request, cts))
			{
				if (reply != null)
				{
					var status = reply.PopString();
					if (status.Equals("200"))
					{
						return reply.Duplicate();
					}
					else if (status.Equals("400"))
					{
						"E: client fatal error, aborting".DumpString();
						cts.Cancel();
					}
					else if (status.Equals("500"))
					{
						"E: server fatal error, aborting".DumpString();
						cts.Cancel();
					}
				}
				else
				{
					cts.Cancel();   // Interrupted or failed
				}
			}
			return null; 
		}
	}
}
