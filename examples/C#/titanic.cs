using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Xml.Serialization;
using ZeroMQ;

namespace Examples
{
	using MDWrkApi;
	using MDCliApi;

	public static class TitanicCommon
	{
		public static readonly string TITANIC_DIR = ".titanic";
		public static readonly string QUEUE_FILE = "queue";
		public static readonly string QUEUE_LINEFORMAT = "-{0}\n";

		public static Guid GenerateUuid()
		{
			return Guid.NewGuid();
		}

		//  Returns request filename for given UUID
		public static string RequestFilename(Guid uuid)
		{
			return string.Format("{0}/{1}.req", TITANIC_DIR, uuid);
		}

		//  Returns reply filename for given UUID
		public static string ReplyFilename(Guid uuid)
		{
			return string.Format("{0}/{1}.rep", TITANIC_DIR, uuid);
		}

		public static void SerializeToXml<T>(this T obj, String path)
			where T : ZMessage
		{
			var msg = ((ZMessage) obj);
			List<byte[]> l = msg.Select(e => e.Read()).ToList();

			XmlSerializer serializer = new XmlSerializer(typeof(List<byte[]>));
			using (StreamWriter writer = new StreamWriter(path))
			{
				serializer.Serialize(writer, l);
			}
		}

		public static T DeserializeFromXml<T>(this String path)
			where T : ZMessage
		{

			ZMessage msg = new ZMessage();
			XmlSerializer serializer = new XmlSerializer(typeof(List<byte[]>));
			using (StreamReader reader = new StreamReader(path))
			{
				var res = (List<byte[]>)serializer.Deserialize(reader);
				foreach (var e in res)
				{
					msg.Add(new ZFrame(e));
				}
			}
			return (T)msg; 
		}

		public static bool TryFileOpenRead(this String path, out FileStream fs)
		{
			fs = null;
			try
			{
				fs = File.OpenRead(path);
				return true;
			}
			catch (Exception ex)
			{
				if (ex is ArgumentException
				 || ex is ArgumentNullException
				 || ex is PathTooLongException
				 || ex is DirectoryNotFoundException
				 || ex is UnauthorizedAccessException
				 || ex is FileNotFoundException
				 || ex is NotSupportedException)
					return false;

				throw;
			}
		}
	}

	static partial class Program
	{

		//  .split Titanic request service
		//  The {{titanic.request}} task waits for requests to this service. It writes
		//  each request to disk and returns a UUID to the client. The client picks
		//  up the reply asynchronously using the {{titanic.reply}} service:
		private static void Titanic_Request(ZContext ctx, ZSocket backendpipe, CancellationTokenSource cancellor, object[] args)
		{
			using (MajordomoWorker worker = new MajordomoWorker("tcp://127.0.0.1:5555", "titanic.request", (bool)args[0]))
			{
				ZMessage reply = null;
				while (true)
				{
					// Send reply if it's not null
					// And then get next request from broker
					ZMessage request = worker.Recv(reply, cancellor);
					if (request == null)
						break; // Interrupted, exit

					// Ensure message directory exists
					Directory.CreateDirectory(TitanicCommon.TITANIC_DIR);
					// Generate UUID and save mesage to disk
					Guid uuid = TitanicCommon.GenerateUuid();
					string fn = TitanicCommon.RequestFilename(uuid);

					request.SerializeToXml(fn);
					request.Dispose();

					// Send UUID through tho message queue
					reply = new ZMessage();
					reply.Add(new ZFrame(uuid.ToString()));
					ZError error;
					if (!backendpipe.Send(reply, out error))
					{
						if(error.Equals(ZError.ETERM))
							break;
					}
					//backendpipe.Send(reply);

					// Now send UUID back to client
					// Done by the mdwrk_recv() at the top of the loop
					reply = new ZMessage();
					reply.Add(new ZFrame("200"));
					reply.Add(new ZFrame(uuid.ToString()));
				}
			}
		}

		//  .split Titanic reply service
		//  The {{titanic.reply}} task checks if there's a reply for the specified
		//  request (by UUID), and returns a 200 (OK), 300 (Pending), or 400
		//  (Unknown) accordingly:
		private static void Titanic_Reply(ZContext context, CancellationTokenSource cts, bool verbose)
		{
			using (var worker = new MajordomoWorker("tcp://127.0.0.1:5555", "titanic.reply", verbose))
			{
				ZMessage reply = null;
				while (true)
				{
					var request = worker.Recv(reply, cts);
					if (request == null)
						break; // Interrupted, exit

					var g = Guid.Parse(request.Pop().ReadString());
					var reqfn = TitanicCommon.RequestFilename(g);
					var repfn = TitanicCommon.ReplyFilename(g);
					if (File.Exists(repfn))
					{
						reply = repfn.DeserializeFromXml<ZMessage>();
						reply.Prepend(new ZFrame("200"));
					}
					else
					{
						reply = new ZMessage();
						if(File.Exists(reqfn))
							reply.Prepend(new ZFrame("300")); //Pending
						else
							reply.Prepend(new ZFrame("400")); //Unknown
					}
					request.Dispose();
				}
			}
		}

		//  .split Titanic close task
		//  The {{titanic.close}} task removes any waiting replies for the request
		//  (specified by UUID). It's idempotent, so it is safe to call more than
		//  once in a row:
		private static void Titanic_Close(ZContext context, CancellationTokenSource cts, bool verbose)
		{
			using (var worker = new MajordomoWorker("tcp://127.0.0.1:5555", "titanic.close", verbose))
			{
				ZMessage reply = null;
				while (true)
				{
					ZMessage request = worker.Recv(reply, cts);
					if (request == null)
						break;

					var g = Guid.Parse(request.Pop().ReadString());
					var reqfn = TitanicCommon.RequestFilename(g);
					var repfn = TitanicCommon.ReplyFilename(g);
					File.Delete(reqfn);
					File.Delete(repfn);
					request.Dispose();
					reply = new ZMessage();
					reply.Add(new ZFrame("200"));
				}
			}
		}


		//  .split try to call a service
		//  Here, we first check if the requested MDP service is defined or not,
		//  using a MMI lookup to the Majordomo broker. If the service exists,
		//  we send a request and wait for a reply using the conventional MDP
		//  client API. This is not meant to be fast, just very simple:

		static bool Titanic_ServiceSuccess(Guid uuid, CancellationTokenSource cts)
		{
			// Load request message, service will be first frame 
			string fn = TitanicCommon.RequestFilename(uuid);
			FileStream fs; 
			if (!fn.TryFileOpenRead(out fs))
				// If the client already close request, treat as successful
				return true;
			fs.Dispose();

			ZMessage request = fn.DeserializeFromXml<ZMessage>();
			var service = request.Pop();
			string servicename = service.ToString();
			bool res = false; 

			// Create MDP client session with short timeout
			using (var client = new MajordomoClient("tcp://127.0.0.1:5555", false)) 
			{
				client.Set_Timeout(1000); // 1sec
				client.Set_Retries(1);    // only 1 retry

				// Use MMI protocol to check if service is available
				ZMessage mmirequest = new ZMessage {service};

				bool service_ok;
				using (var mmireply = client.Send("mmi.service", mmirequest, cts))
					service_ok = (mmireply != null
								  && mmireply.First().ToString().Equals("200"));

				res = false;
				if(service_ok)
					using (ZMessage reply = client.Send(servicename, request, cts)) 
						if (reply != null)
						{
							fn = TitanicCommon.ReplyFilename(uuid);
							reply.SerializeToXml(fn);
							res = true; 
						}
				else
					request.Dispose();
			}
			return res; 
		}

		//  Titanic service
		//  Implements server side of http://rfc.zeromq.org/spec:9
		public static void Titanic(string[] args)
		{
			bool verbosedeep =
						   (args.Any(e => e.ToLower().Equals("-vd")
										  || e.ToLower().Equals("--verbosedeep")));


			bool verbose = verbosedeep || (args.Any(e => e.ToLower().Equals("-v")
													  || e.ToLower().Equals("--verbose")));

			Console.WriteLine("Verbose: {0}", verbose);
			Console.WriteLine("Verbosedeep: {0}", verbosedeep);

			CancellationTokenSource cancellor = new CancellationTokenSource();
			Console.CancelKeyPress += (s, ea) =>
			{
				ea.Cancel = true;
				cancellor.Cancel();
			};

			ZContext ctx = new ZContext();
			using (var requestPipe = new ZActor(ctx, Titanic_Request, verbosedeep))
			{
				(new Thread(() => Titanic_Reply(ctx, cancellor, verbosedeep))).Start();
				(new Thread(() => Titanic_Close(ctx, cancellor, verbosedeep))).Start();
				////////////////////
				/// HINT: Use requestPipe.Start instead of requestPipe.Start(cancellor) 
				/// => with cancellor consturctor needed frontent pipe will not be initializes!!
				////////////////////
				requestPipe.Start(); 
				Thread.Sleep(1500);


				// Main dispatcher loop
				while (true)
				{
					//continue;
					if (cancellor.IsCancellationRequested
					|| (Console.KeyAvailable && Console.ReadKey(true).Key == ConsoleKey.Escape))
						ctx.Shutdown();

					var path = Path.Combine(TitanicCommon.TITANIC_DIR, TitanicCommon.QUEUE_FILE);
					var p = ZPollItem.CreateReceiver();
					ZMessage msg;
					ZError error;
					if (requestPipe.Frontend.PollIn(p, out msg, out error, TimeSpan.FromMilliseconds(1000)))
					{
						using (msg)
						{
							// Ensure message directory exists
							Directory.CreateDirectory(TitanicCommon.TITANIC_DIR);
								
							// Append UUID to queue, prefixed with '-' for pending
							var uuid = Guid.Parse(msg.PopString());
							using (var sw = File.AppendText(path))
							{
								sw.Write(TitanicCommon.QUEUE_LINEFORMAT, uuid);
							}
						}
					}
					else if (error.Equals(ZError.ETERM))
					{
						cancellor.Cancel();
						break; // Interrupted
					}
					else if (error.Equals(ZError.EAGAIN))
						//continue;
					{
						Thread.Sleep(1);
					}
					else
						break; // Interrupted

					// Brute force dispatcher
					if(File.Exists(path))
						using (FileStream fs = File.Open(path, FileMode.Open, FileAccess.ReadWrite, FileShare.ReadWrite))
						{
							int numBytesRead = 0;
							int numBytesToRead = (new UTF8Encoding().GetBytes(String.Format(TitanicCommon.QUEUE_LINEFORMAT, Guid.NewGuid()))).Length;
							byte[] readBytes = new byte[numBytesToRead];
							while (numBytesToRead > 0)
							{
								var n = fs.Read(readBytes, 0, numBytesToRead);
								if (n == 0)
									break;
								var line = (new UTF8Encoding()).GetString(readBytes, 0, n);
								//  UUID is prefixed with '-' if still waiting
								if (line.StartsWith("-"))
								{
									var uuid = Guid.Parse(line.Substring(1, Guid.NewGuid().ToString().Length));
									if (verbose)
										"I: processing request {0}".DumpString(uuid);
									if (Titanic_ServiceSuccess(uuid, cancellor))
									{
										//  Mark queue entry as processed
										var newval = (new UTF8Encoding()).GetBytes("+");
										fs.Seek(-n, SeekOrigin.Current);
										fs.Write(newval, 0, newval.Length);
										fs.Seek(n - newval.Length, SeekOrigin.Current);
									}
								}
								if (cancellor.IsCancellationRequested)
									break; 

								numBytesRead += n;
								numBytesToRead = n;
							}
						}
				}
			}
		}
	}
}
