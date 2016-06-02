using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Runtime.Serialization;
using System.Threading;

namespace Examples
{
	static class ProgramRunner
	{
		static int Main(string[] args)
		{
			// REAL
			var fields = typeof(Program).GetFields(BindingFlags.Public | BindingFlags.Static).OrderBy(field => field.Name).ToList();

			int leaveOut = 0;
			var dict = new Dictionary<string, string>();
			if (args != null && args.Length > 0)
			{
				foreach (string arg in args)
				{
					if (arg.StartsWith("--"))
					{
						leaveOut++;

						int iOfEquals = arg.IndexOf('=');
						string key, value;
						if (-1 < iOfEquals)
						{
							key = arg.Substring(0, iOfEquals);
							value = arg.Substring(iOfEquals + 1);
						}
						else {
							key = arg.Substring(0);
							value = null;
						}
						dict.Add(key, value);

						FieldInfo keyField = fields.Where(field => string.Equals(field.Name, key.Substring(2), StringComparison.OrdinalIgnoreCase)).FirstOrDefault();
						if (keyField != null)
						{
							if (keyField.FieldType == typeof(string))
							{
								keyField.SetValue(null, value);
							}
							else if (keyField.FieldType == typeof(bool))
							{
								bool equalsTrue = (value == null || value == string.Empty);
								if (!equalsTrue)
									equalsTrue = string.Equals(value, "true", StringComparison.OrdinalIgnoreCase);
								if (!equalsTrue)
									equalsTrue = string.Equals(value, "+", StringComparison.OrdinalIgnoreCase);

								keyField.SetValue(null, equalsTrue);
							}
						}
					}
				}
			}

			int returnMain = 0;
			string command = (args == null || args.Length == 0) ? "help" : args[0 + leaveOut].ToLower();

			var methods = typeof(Program).GetMethods(BindingFlags.Public | BindingFlags.Static).OrderBy(method => method.Name).ToList();
			if (command != "help")
			{

				var method = methods.FirstOrDefault(m => m.Name.Equals(command, StringComparison.OrdinalIgnoreCase));
				if (method != null)
				{

					ParameterInfo[] methodParameters = method.GetParameters();

					object[] parameters;

					if (methodParameters.Length == 2)
					{
						parameters = new object[] { 
							dict,
							args.Skip(1 + leaveOut).ToArray() /* string[] args */
						};
					}
					else if (methodParameters.Length == 1)
					{
						parameters = new object[] { 
							args.Skip(1 + leaveOut).ToArray() /* string[] args */
						};
					}
					else
					{
						throw new InvalidOperationException();
					}

					// INFO: Invoking the Sample by "the Delegate.Invoke" makes it hard to debug!
					// Using DebugInvoke
					object result
						= DebugStackTrace<TargetInvocationException>.Invoke(
							method,
							null,
							parameters);

					if (method.ReturnType == typeof(bool) && true == (bool)result)
					{
						return 0; // C good
					}

					return -1; // C bad
				}

				returnMain = -1;
				Console.WriteLine();
				Console.WriteLine("Command invalid.");
			}

			Console.WriteLine();
			Console.WriteLine("Usage: ./" + AppDomain.CurrentDomain.FriendlyName + " [--option] <command> World");

			if (fields.Count > 0)
			{
				Console.WriteLine();
				Console.WriteLine("Available [option]s:");
				Console.WriteLine();
				foreach (FieldInfo field in fields)
				{
					Console.WriteLine("  --{0}", field.Name);
				}
			}

			Console.WriteLine();
			Console.WriteLine("Available <command>s:");
			Console.WriteLine();

			foreach (MethodInfo method in methods)
			{
				if (method.Name == "Main")
					continue;
				if (0 < method.GetCustomAttributes(typeof(System.Runtime.CompilerServices.CompilerGeneratedAttribute), true).Length)
					continue;

				Console.WriteLine("    {0}", method.Name);
			}

			Console.WriteLine();
			return returnMain;
		}
	}


	internal static class DebugStackTrace<TException>
		where TException : Exception
	{
		[System.Diagnostics.DebuggerNonUserCode]
		[System.Diagnostics.DebuggerStepThrough]
		public static object Invoke(MethodInfo method, object target, params object[] args)
		{
			// source : http://csharptest.net/350/throw-innerexception-without-the-loosing-stack-trace/

			try
			{
				return method.Invoke(target, args);
			}
			catch (TException te)
			{
				if (te.InnerException == null)
					throw te;

				Exception innerException = te.InnerException;

				var savestack = (ThreadStart)Delegate.CreateDelegate(typeof(ThreadStart), innerException, "InternalPreserveStackTrace", false, false);
				if (savestack != null) savestack();

				throw innerException; // -- now we can re-throw without trashing the stack /**/

				// PreserveStackTrace(te);
				// throw te;
			}
		}

		// http://stackoverflow.com/a/2085377/1352471 (Anton Tykhyy on In C#, how can I rethrow InnerException without losing stack trace?)
		static void PreserveStackTrace(Exception e)
		{
			var ctx = new StreamingContext(StreamingContextStates.CrossAppDomain);
			var mgr = new ObjectManager(null, ctx);
			var si = new SerializationInfo(e.GetType(), new FormatterConverter());

			e.GetObjectData(si, ctx);
			mgr.RegisterObject(e, 1, si); // prepare for SetObjectData
			mgr.DoFixups(); // ObjectManager calls SetObjectData

			// voila, e is unmodified save for _remoteStackTraceString
		}

	}
}
