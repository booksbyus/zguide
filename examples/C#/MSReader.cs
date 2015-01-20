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
		public static void MSReader(IDictionary<string, string> dict, string[] args)
		{
			using (var context = ZContext.Create())
			using (var receiver = ZSocket.Create(context, ZSocketType.PULL))
			using (var subscriber = ZSocket.Create(context, ZSocketType.SUB))
			{
				receiver.Connect("tcp://127.0.0.1:5557");

				subscriber.Connect("tcp://127.0.0.1:5556");
				subscriber.SetOption(ZSocketOption.SUBSCRIBE, "72622 ");

				ZError error;
				ZFrame frame;
				while (true)
				{
					if (null != (frame = receiver.ReceiveFrame(ZSocketFlags.DontWait, out error)))
					{
						// Process task
					} 
					if (null != (frame = subscriber.ReceiveFrame(ZSocketFlags.DontWait, out error)))
					{
						// Process weather update
					} 
					Thread.Sleep(1);
				}
			}
		}
	}
}