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

				int published = 0;
				while (true)
				{
					// Write two messages, each with an envelope and content

					using (var message = new ZMessage())
					{
						published++;
						message.Add(new ZFrame(string.Format("A {0}", published)));
						message.Add(new ZFrame(string.Format(" We don't like to see this.")));
						Thread.Sleep(1000);

						Console_WriteZMessage("Publishing ", message);
						publisher.Send(message);
					}

					using (var message = new ZMessage())
					{
						published++;
						message.Add(new ZFrame(string.Format("B {0}", published)));
						message.Add(new ZFrame(string.Format(" We do like to see this.")));
						Thread.Sleep(1000);

						Console_WriteZMessage("Publishing ", message);
						publisher.Send(message);
					}
				}
			}
		}
	}
}