using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

using ZeroMQ;
using ZeroMQ.lib;

namespace ZeroMQ.Test
{
	static partial class Program
	{
		public static void Version(IDictionary<string, string> dict, string[] args)
		{
			//
			// Report 0MQ version
			//
			// Authors: Uli Riehm
			//

			// Console.WriteLine(zmq.Version);

			int major, minor, patch;
			zmq.version(out major, out minor, out patch);
			Console.WriteLine("{0}.{1}.{2}", major, minor, patch);
		}
	}
}