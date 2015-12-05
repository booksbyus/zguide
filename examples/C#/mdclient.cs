using System;
using System.Linq;
using System.Threading;
using ZeroMQ;

namespace Examples
{
	using MDCliApi; // Let us build this source without creating a library
	static partial class Program
	{
		//  Majordomo Protocol client example
		//  Uses the mdcli API to hide all MDP aspects
		public static void MDClient(string[] args)
		{
			bool verbose = (args.Any(e => e.ToLower().Equals("-v")
									   || e.ToLower().Equals("--verbose")));
			Console.WriteLine("Verbose: {0}", verbose);

			CancellationTokenSource cts = new CancellationTokenSource();
			Console.CancelKeyPress += (s, ea) =>
			{
				ea.Cancel = true;
				cts.Cancel();
			};

			using (MajordomoClient session = new MajordomoClient("tcp://127.0.0.1:5555", verbose))
			{
				int count;
				for (count = 0; count < 100000; count++)
				{
					ZMessage request = new ZMessage();
					request.Prepend(new ZFrame("Hello world"));
					using (ZMessage reply = session.Send("echo", request, cts))
						if (reply == null)
							break; // Interrupt or failure
				}
				Console.WriteLine("{0} requests/replies processed\n", count);
			}
		}
	}
}
