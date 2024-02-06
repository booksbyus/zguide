using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.NetworkInformation;
using System.Net.Sockets;
using System.Threading;

using ZeroMQ;

namespace Examples
{
	static partial class Program
	{
		public static void WUProxy(string[] args)
		{
			//
			// Weather proxy device
			//
			// Author: metadings
			//

			using (var context = new ZContext())
			using (var frontend = new ZSocket(context, ZSocketType.XSUB))
			using (var backend = new ZSocket(context, ZSocketType.XPUB))
			{
				// Frontend is where the weather server sits
				string localhost = "tcp://127.0.0.1:5556";
				Console.WriteLine("I: Connecting to {0}", localhost);
				frontend.Connect(localhost);

				// Backend is our public endpoint for subscribers
				foreach (IPAddress address in WUProxy_GetPublicIPs())
				{
					var tcpAddress = string.Format("tcp://{0}:8100", address);
					Console.WriteLine("I: Binding on {0}", tcpAddress);
					backend.Bind(tcpAddress);

					var epgmAddress = string.Format("epgm://{0};239.192.1.1:8100", address);
					Console.WriteLine("I: Binding on {0}", epgmAddress);
					backend.Bind(epgmAddress);
				}
				using (var subscription = ZFrame.Create(1))
				{
					subscription.Write(new byte[] { 0x1 }, 0, 1);
					backend.Send(subscription);
				}

				// Run the proxy until the user interrupts us
				ZContext.Proxy(frontend, backend);
			}
		}

		static IEnumerable<IPAddress> WUProxy_GetPublicIPs() 
		{
			var list = new List<IPAddress>();
			NetworkInterface[] ifaces = NetworkInterface.GetAllNetworkInterfaces();
			foreach (NetworkInterface iface in ifaces)
			{
				if (iface.NetworkInterfaceType == NetworkInterfaceType.Loopback)
					continue;
				if (iface.OperationalStatus != OperationalStatus.Up)
					continue;

				var props = iface.GetIPProperties();
				var addresses = props.UnicastAddresses;
				foreach (UnicastIPAddressInformation address in addresses)
				{
					if (address.Address.AddressFamily == AddressFamily.InterNetwork)
						list.Add(address.Address);
					// if (address.Address.AddressFamily == AddressFamily.InterNetworkV6)
					//	list.Add(address.Address);
				}
			}
			return list;
		}
	}
}