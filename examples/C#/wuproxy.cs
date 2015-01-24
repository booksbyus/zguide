using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.NetworkInformation;
using System.Net.Sockets;
using System.Threading;

using ZeroMQ;

namespace ZeroMQ.Test
{
	static partial class Program
	{
		public static void WUProxy(IDictionary<string, string> dict, string[] args)
		{
			//
			// Weather proxy device
			//
			// Authors: Pieter Hintjens, Uli Riehm
			//

			using (var context = ZContext.Create())
			using (var frontend = ZSocket.Create(context, ZSocketType.XSUB))
			using (var backend = ZSocket.Create(context, ZSocketType.XPUB))
			{
				// Frontend is where the weather server sits
				frontend.Connect("tcp://127.0.0.1:5556");

				// Backend is our public endpoint for subscribers
				foreach (IPAddress address in WUProxy_GetPublicIPs())
				{
					Console.WriteLine("I: Binding on {0}", address);
					backend.Bind(string.Format("tcp://{0}:8100", address));
					backend.Bind(string.Format("epgm://{0};239.192.1.1:8100", address));
				}
				using (var subscription = new ZFrame())
				{
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