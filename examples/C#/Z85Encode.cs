using System;
using System.Collections.Generic;
using System.Threading;

using ZeroMQ;

namespace Examples
{
	public partial class Program
	{
		public static void Z85Encode(IDictionary<string, string> dict, string[] args)
		{
			//
			// Author: metadings
			//

			if (args == null || args.Length < 1)
			{
				args = new string[] { "Hello World!" };
			}

			string txt = args[0];
			string encoded = Z85.Encode(txt);
			Console.WriteLine("{0}: {1}", txt, encoded);
		}

		public static void Z85Decode(IDictionary<string, string> dict, string[] args)
		{
			//
			// Author: metadings
			//

			if (args == null || args.Length < 1)
			{
				args = new string[] { "nm=QNzY&b1A+]nf" };
			}

			string txt = args[0];
			string decoded = Z85.Decode(txt);
			Console.WriteLine(decoded);
		}
	}
}

