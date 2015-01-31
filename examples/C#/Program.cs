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
		// INFO: You will find a "static int Main(string[] args)" in ProgramRunner.cs

		public static bool Verbose = false;

		static void Console_WriteZFrame(ZFrame frame, string format, params object[] data)
		{
			var renderer = new StringBuilder();

			var list = new List<object>(data);

			// here the renderer

			renderer.Append(format);
			renderer.Append(": ");
			renderer.Append("{");
			renderer.Append(0 + data.Length);
			renderer.Append("}");

			// now the message

			frame.Position = 0;

			if (frame.Length == 0)
				list.Add("0");
			else
				list.Add(frame.ReadString());

			frame.Position = 0;

			Console.WriteLine(renderer.ToString(), list.ToArray());
		}

		static void Console_WriteZMessage(ZMessage message, string format, params object[] data)
		{
			var renderer = new StringBuilder();

			var list = new List<object>(data);

			for (int i = 0, c = message.Count; i < c; ++i)
			{
				// here the renderer
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
				renderer.Append( i + data.Length );
				renderer.Append("}");

				// now the message
				ZFrame frame = message[i];

				frame.Position = 0;

				if (frame.Length == 0)
					list.Add("0");
				else
					list.Add(frame.ReadString());

				frame.Position = 0;
			}

			Console.WriteLine(renderer.ToString(), list.ToArray());
		}
	}

	public static class Ext {

		public static string ToHexString(this byte[] hex) {
			if (hex == null) {
				return null;
			}
			if (hex.Length == 0) {
				return string.Empty;
			}
			var s = new StringBuilder();
			foreach (byte b in hex) {
				s.Append(b.ToString("x2"));
			}
			return s.ToString();
		}

		public static byte[] ToHexBytes(this string hex)
		{
			if (hex == null) {
				return null;
			}
			if (hex.Length == 0) {
				return new byte[0];
			}
			int l = hex.Length / 2;
			var b = new byte[l];
			for (int i = 0; i < l; ++i) {
				b[i] = Convert.ToByte(hex.Substring(i * 2, 2), 16);
			}
			return b;
		}
	}
}