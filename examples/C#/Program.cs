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
		static void Console_WriteZMessage(ZMessage message, string format, params object[] data) 
		{
			var list = new List<string>();
			var renderer = new StringBuilder();
			for (int i = 0, c = message.Count; i < c; ++i)
			{
				ZFrame frame = message[i];
				frame.Position = 0;

				if (i == 0)
				{
					renderer.Append(format);
					renderer.Append(": ");
				}
				else
				{
					renderer.Append(", ");
				}
				renderer.Append("{");
				renderer.Append(i + data.Length);
				renderer.Append("}");

				if (frame.Length == 0)
				{
					list.Add("0");
				}
				else
				{
					list.Add(frame.ReadString());
				}
			}

			Console.WriteLine(renderer.ToString(), list.ToArray());
		}
	}
}