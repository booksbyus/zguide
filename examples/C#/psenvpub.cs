using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

using ZeroMQ;

namespace Examples
{
	static partial class Program
	{
		public static void PSEnvPub(string[] args)
		{
			//
			// Pubsub envelope publisher
			//
			// Author: metadings
			//

			// Prepare our context and publisher
			using (var context = new ZContext())
			using (var publisher = new ZSocket(context, ZSocketType.PUB))
			{
				publisher.Linger = TimeSpan.Zero;
				publisher.Bind("tcp://*:5563");

				int publishing = 0;
				while (true)
				{
					publishing++;

					// Write two messages, each with an envelope and content
					using (var message = new ZMessage())
					{
						message.Add(new ZFrame("A"));
						message.Add(new ZFrame(string.Format(" is {0}: we don't like to see this", publishing)));

						Console_WriteZMessage("Publishing ", message);
						publisher.Send(message);
					}
					using (var message = new ZMessage())
					{
						message.Add(new ZFrame("B"));
						message.Add(new ZFrame(string.Format(" is {0}: WE do like to see this", publishing)));

						Console_WriteZMessage("Publishing ", message);
						publisher.Send(message);
					}
					Thread.Sleep(1000);
				}
			}
		}
	}
}