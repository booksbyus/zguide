using System;
using System.Linq;
using System.Threading;
using ZeroMQ;

namespace Examples
{
	using MDWrkApi; // Let us build this source without creating a library

	static partial class Program
	{
		//  Majordomo Protocol worker example
		//  Uses the mdwrk API to hide all MDP aspects
		public static void MDWorker(string[] args)
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

			using (MajordomoWorker session = new MajordomoWorker("tcp://127.0.0.1:5555", "echo", verbose))
			{
				ZMessage reply = null;
				while (true)
				{
					ZMessage request = session.Recv(reply, cts);
					if (request == null)
						break; // worker was interrupted
					reply = request; // Echo is complex
				}
			}
		}
	}
}
