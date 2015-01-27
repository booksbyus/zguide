using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

using ZeroMQ;

namespace ZeroMQ.Test
{
	using FLCli;

	//
	// flcliapi class - Freelance Pattern agent class
	// Implements the Freelance Protocol at http://rfc.zeromq.org/spec:10
	//
	// Authors: Pieter Hintjens, Uli Riehm
	//

	// This API works in two halves, a common pattern for APIs that need to
	// run in the background. One half is an frontend object our application
	// creates and works with; the other half is a backend "agent" that runs
	// in a background thread. The frontend talks to the backend over an
	// inproc pipe socket:

	namespace FLCli
	{
		public class FLCliApi
		{
			// Our context wrapper
			ZContext context;

			ZActor actor;

			// Pipe through to flcliapi agent
			ZSocket pipe;

			public FLCliApi()
			{
				context = ZContext.Create();

				// actor = ZActor.Create(context, this.Agent).Start();
				// pipe = actor.Frontend;
			}

			public void Agent(object[] args, ZSocket backend)
			{
				// Finally, here's the agent task itself, which polls its two sockets
				// and processes incoming messages:

				throw new NotImplementedException();

			}
		}
	}

	static partial class Program
	{

		public static void FLCliApi(IDictionary<string, string> dict, string[] args)
		{
			throw new NotImplementedException();
		}
	}
}