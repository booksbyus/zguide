using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using ZeroMQ;

namespace Examples
{

	public static class MdpCommon
	{
		public const int HEARTBEAT_LIVENESS = 3;

		public static readonly TimeSpan HEARTBEAT_DELAY = TimeSpan.FromMilliseconds(2500);
		public static readonly TimeSpan RECONNECT_DELAY = TimeSpan.FromMilliseconds(2500);

		public static readonly TimeSpan HEARTBEAT_INTERVAL = TimeSpan.FromMilliseconds(2500);
		public static readonly TimeSpan HEARTBEAT_EXPIRY =
			TimeSpan.FromMilliseconds(HEARTBEAT_INTERVAL.TotalMilliseconds * HEARTBEAT_LIVENESS);

		public static readonly string MDPW_WORKER = "MDPW01";
		public static readonly string MDPC_CLIENT = "MDPC01";

		//public static readonly string READY = "001";
		//public static readonly string REQUEST = "002";
		//public static readonly string REPLY = "003";
		//public static readonly string HEARTBEAT = "004";
		//public static readonly string DISCONNECT = "005";

		public enum MdpwCmd : byte { READY = 1, REQUEST = 2, REPLY = 3, HEARTBEAT = 4, DISCONNECT = 5 }
	}

	public static class MdpExtensions
	{
		public static bool StrHexEq(this ZFrame zfrm, MdpCommon.MdpwCmd cmd)
		{
			return zfrm.ToString().ToMdCmd().Equals(cmd);
		}

		/// <summary>
		/// Parse hex value to MdpwCmd, if parsing fails, return 0
		/// </summary>
		/// <param name="hexval">hex string</param>
		/// <returns>MdpwCmd, return 0 if parsing failed</returns>
		public static MdpCommon.MdpwCmd ToMdCmd(this string hexval)
		{
			try
			{
				MdpCommon.MdpwCmd cmd = (MdpCommon.MdpwCmd)byte.Parse(hexval, NumberStyles.AllowHexSpecifier);
				return cmd;
			}
			catch (FormatException)
			{
				return 0;
			}
		}

		public static string ToHexString(this MdpCommon.MdpwCmd cmd)
		{
			return cmd.ToString("X");
		}

		public static void DumpString(this string format, params object[] args)
		{
			// if you dont wanna see utc timeshift, remove zzz and use DateTime.UtcNow instead
			Console.WriteLine("[{0}] - {1}", string.Format("{0:yyyy-MM-ddTHH:mm:ss:fffffff zzz}", DateTime.Now), string.Format(format, args));
		}

		/// <summary>
		/// Based on zmsg_dump 
		/// https://github.com/imatix/zguide/blob/f94e8995a5e02d843434ace904a7afc48e266b3f/articles/src/multithreading/zmsg.c
		/// </summary>
		/// <param name="zmsg"></param>
		/// <param name="format"></param>
		/// <param name="args"></param>
		public static void DumpZmsg(this ZMessage zmsg, string format = null, params object[] args)
		{
			if (!string.IsNullOrWhiteSpace(format))
				format.DumpString(args);
			using (var dmsg = zmsg.Duplicate())
				foreach (var zfrm in dmsg)
				{
					zfrm.DumpZfrm();
				}
		}

		public static void DumpZfrm(this ZFrame zfrm, string format = null, params object[] args)
		{
			if(!string.IsNullOrWhiteSpace(format))
				format.DumpString(args);

			byte[] data = zfrm.Read();
			long size = zfrm.Length;

			// Dump the message as text or binary
			bool isText = true;
			for (int i = 0; i < size; i++)
				if (data[i] < 32 || data[i] > 127)
					isText = false;
			string datastr = isText ? Encoding.UTF8.GetString(data) : data.ToHexString();
			"\tD: [{0,3:D3}]:{1}".DumpString(size, datastr);
		}
	}

}
