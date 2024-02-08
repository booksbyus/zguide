using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

using ZeroMQ;
using ZeroMQ.lib;

namespace Examples
{
	static partial class Program
	{
		public static void Version(string[] args)
		{
			//
			// Report 0MQ version
			//
			// Author: metadings
			//

			// Console.WriteLine(zmq.Version);

			int major, minor, patch;
			zmq.version(out major, out minor, out patch);
			Console.WriteLine("{0}.{1}.{2}", major, minor, patch);
		}
	}
}