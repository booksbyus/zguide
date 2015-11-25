using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading;
using ZeroMQ;

namespace Examples
{
   using MDBroker; 

    namespace MDBroker
    {
        public class Broker : IDisposable
        {
            //  Majordomo Protocol broker
            //  A minimal C implementation of the Majordomo Protocol as defined in
            //  http://rfc.zeromq.org/spec:7 and http://rfc.zeromq.org/spec:8.
            
            //  .split broker class structure
            //  The broker class defines a single broker instance:

            // Our Context
            private ZContext _context;

            //Socket for clients & workers
            public ZSocket Socket;

            //  Print activity to console
            public bool Verbose { get; protected set; }

            //  Broker binds to this endpoint
            public string Endpoint { get; protected set; }

            // Hash of known services
            public Dictionary<string, Service> Services;

            // Hash of known workes
            public Dictionary<string, Worker> Workers;

            // Waiting workers
            public List<Worker> Waiting;

            // When to send HEARTBEAT
            public DateTime HeartbeatAt;

            // waiting
            // heartbeat_at

            public Broker(bool verbose)
            {
                // Constructor
                _context = new ZContext();

                Socket = new ZSocket(_context, ZSocketType.ROUTER);

                Verbose = verbose;

                Services = new Dictionary<string, Service>(); //new HashSet<Service>();
                Workers = new Dictionary<string, Worker>(); //new HashSet<Worker>();
                Waiting = new List<Worker>();

                HeartbeatAt = DateTime.UtcNow + MdpCommon.HEARTBEAT_INTERVAL;
            }

            ~Broker()
            {
                Dispose(false);
            }

            public void Dispose()
            {
                GC.SuppressFinalize(this);
                Dispose(true);
            }

            protected void Dispose(bool disposing)
            {
                if (disposing)
                {
                    // Destructor
                    if (Socket != null)
                    {
                        Socket.Dispose();
                        Socket = null;
                    }

                    if (_context != null)
                    {
                        // Do Context.Dispose()
                        _context.Dispose();
                        _context = null;
                    }
                }
            }

            // added ShutdownContext, to call shutdown in main method.
            public void ShutdownContext()
            {
                if (_context != null)
                    _context.Shutdown();
            }

            /*
            static void s_broker_bind(broker_t* self, char* endpoint);
            static void s_broker_worker_msg(broker_t* self, zframe_t* sender, zmsg_t* msg);
            static void s_broker_client_msg(broker_t* self, zframe_t* sender, zmsg_t* msg);
            static void s_broker_purge(broker_t* self);
            */

            //  .split broker bind method
            //  This method binds the broker instance to an endpoint. We can call
            //  this multiple times. Note that MDP uses a single Socket for both clients 
            //  and workers:
            public void Bind(string endpoint)
            {
                Socket.Bind(endpoint);
                // if you dont wanna see utc timeshift, remove zzz and use DateTime.UtcNow instead
                "I: MDP broker/0.2.0 is active at {0}".DumpString(endpoint);
            }

            //  .split broker worker_msg method
            //  This method processes one READY, REPLY, HEARTBEAT, or
            //  DISCONNECT message sent to the broker by a worker:
            public void WorkerMsg(ZFrame sender, ZMessage msg)
            {
                if(msg.Count < 1) // At least, command
                    throw new InvalidOperationException();

                ZFrame command = msg.Pop();
                //string id_string = sender.ReadString();
                bool isWorkerReady; 
                //string id_string;
                using (var sfrm = sender.Duplicate())
                {
                    var idString = sfrm.Read().ToHexString();
                    isWorkerReady = Workers.ContainsKey(idString);
                }
                Worker worker = RequireWorker(sender);
                using (msg)
                {
                    using (command)
                    {
                        
                        if (command.StrHexEq(MdpCommon.MdpwCmd.READY))
                        {
                            if (isWorkerReady)
                                // Not first command in session
                                worker.Delete(true);
                            else if (command.Length >= 4
                                     && command.ToString().StartsWith("mmi."))
                                // Reserd servicee name
                                worker.Delete(true);
                            else
                            {
                                // Attach worker to service and mark as idle
                                using (ZFrame serviceFrame = msg.Pop())
                                {
                                    worker.Service = RequireService(serviceFrame);
                                    worker.Service.Workers++;
                                    worker.Waiting();
                                }
                            }
                        }
                        else if (command.StrHexEq(MdpCommon.MdpwCmd.REPLY))
                        {
                            if (isWorkerReady)
                            {
                                //  Remove and save client return envelope and insert the
                                //  protocol header and service name, then rewrap envelope.
                                ZFrame client = msg.Unwrap();
                                msg.Prepend(new ZFrame(worker.Service.Name));
                                msg.Prepend(new ZFrame(MdpCommon.MDPC_CLIENT));
                                msg.Wrap(client);
                                Socket.Send(msg);
                                worker.Waiting();
                            }
                            else
                            {
                                worker.Delete(true);
                            }
                        }
                        else if (command.StrHexEq(MdpCommon.MdpwCmd.HEARTBEAT))
                        {
                            if (isWorkerReady)
                            {
                                worker.Expiry = DateTime.UtcNow + MdpCommon.HEARTBEAT_EXPIRY;
                            }
                            else
                            {
                                worker.Delete(true);
                            }
                        }
                        else if (command.StrHexEq(MdpCommon.MdpwCmd.DISCONNECT))
                            worker.Delete(false);
                        else
                        {
                            msg.DumpZmsg("E: invalid input message");
                        }
                    }
                }
            }

            //  .split broker client_msg method
            //  Process a request coming from a client. We implement MMI requests
            //  directly here (at present, we implement only the mmi.service request):
            public void ClientMsg(ZFrame sender, ZMessage msg)
            {
                // service & body
                if(msg.Count < 2)
                    throw new InvalidOperationException();

                using (ZFrame serviceFrame = msg.Pop())
                {
                    Service service = RequireService(serviceFrame);

                    // Set reply return identity to client sender
                    msg.Wrap(sender.Duplicate());

                    //if we got a MMI Service request, process that internally
                    if (serviceFrame.Length >= 4
                        && serviceFrame.ToString().StartsWith("mmi."))
                    {
                        string returnCode;
                        if (serviceFrame.ToString().Equals("mmi.service"))
                        {
                            string name = msg.Last().ToString();
                            returnCode = Services.ContainsKey(name) 
                                         && Services[name].Workers > 0
                                            ? "200"
                                            : "400";
                        }
                        else
                            returnCode = "501";

                        using (var resetableFrame = msg.Pop())
                        {
                            msg.Prepend(new ZFrame(returnCode));
                        }

                        //ToDo check c implementation
                        throw new NotImplementedException("ToDo: fix this section, never tested. contains errors mmi services never called with the mdclient/mdbroker/mdworker examples");
                        //## following code has some errors

                        //  Remove & save client return envelope and insert the
                        //  protocol header and Service name, then rewrap envelope.
                        ZFrame client = msg.Unwrap();
                        msg.Prepend(serviceFrame);
                        msg.Prepend(new ZFrame(MdpCommon.MDPC_CLIENT));
                        msg.Wrap(client);
                        Socket.Send(msg);
                    }
                    else
                    {
                        // Else dispatch the message to the requested Service
                        service.Dispatch(msg);
                    }
                }
            }

            //  .split broker purge method
            //  This method deletes any idle workers that haven't pinged us in a
            //  while. We hold workers from oldest to most recent so we can stop
            //  scanning whenever we find a live worker. This means we'll mainly stop
            //  at the first worker, which is essential when we have large numbers of
            //  workers (we call this method in our critical path):

            public void Purge()
            {
                Worker worker = Waiting.FirstOrDefault();
                while (worker != null)
                {
                    if (DateTime.UtcNow < worker.Expiry)
                        break;   // Worker is alive, we're done here
                    if(Verbose)
                        "I: deleting expired worker: '{0}'".DumpString(worker.IdString);
                    
                    worker.Delete(false);
                    worker = Waiting.FirstOrDefault();
                }
            }

            //  .split service methods
            //  Here is the implementation of the methods that work on a service:

            //  Lazy constructor that locates a service by name or creates a new
            //  service if there is no service already with that name.

            public Service RequireService(ZFrame serviceFrame)
            {
                if(serviceFrame == null)
                    throw new InvalidOperationException();

                string name = serviceFrame.ToString();

                Service service;
                if (Services.ContainsKey(name))
                {
                    service = Services[name];
                }
                else
                {
                    service = new Service(this, name);
                    Services[name] = service;

                    //zhash_freefn(self->workers, id_string, s_worker_destroy);
                    if (Verbose)
                        "I: added service: '{0}'".DumpString(name);
                }

                return service;
            }

            //  .split worker methods
            //  Here is the implementation of the methods that work on a worker:

            //  Lazy constructor that locates a worker by identity, or creates a new
            //  worker if there is no worker already with that identity.

            public Worker RequireWorker(ZFrame identity)
            {
                if (identity == null)
                    throw new InvalidOperationException();

                string idString;
                using (var tstfrm = identity.Duplicate())
                {
                    idString = tstfrm.Read().ToHexString();
                }
                
                Worker worker = Workers.ContainsKey(idString)
                    ? Workers[idString]
                    : null;

                if (worker == null)
                {
                    worker = new Worker(idString, this, identity);
                    Workers[idString] = worker;
                    if(Verbose)
                        "I: registering new worker: '{0}'".DumpString(idString);
                }
                
                return worker;
            }
        }

        public class Service : IDisposable
        {
            // Broker Instance
            public Broker Broker { get; protected set; }

            // Service Name
            public string Name { get; protected set; }

            // List of client requests
            public List<ZMessage> Requests { get; protected set; }

            // List of waiting workers
            public List<Worker> Waiting { get; protected set; }

            // How many workers we are
            public int Workers;
            //ToDo check workers var

            internal Service(Broker broker, string name)
            {
                Broker = broker;
                Name = name; 
                Requests = new List<ZMessage>();
                Waiting = new List<Worker>();
            }

            ~Service()
            {
                Dispose(false);
            }

            public void Dispose()
            {
                GC.SuppressFinalize(this);
                Dispose(true);
            }

            protected void Dispose(bool disposing)
            {
                if (disposing)
                {
                    foreach (var r in Requests)
                    {
                        // probably obsolete?
                        using (r)
                        {
                        }
                    }
                }
            }

            //  .split service dispatch method
            //  This method sends requests to waiting workers:
            public void Dispatch(ZMessage msg)
            {
                if (msg != null) // queue msg if any
                    Requests.Add(msg);

                Broker.Purge();
                while (Waiting.Count > 0
                       && Requests.Count > 0)
                {
                    Worker worker = Waiting[0];
                    Waiting.RemoveAt(0);
                    Broker.Waiting.Remove(worker);
                    ZMessage reqMsg = Requests[0];
                    Requests.RemoveAt(0);
                    using (reqMsg)
                        worker.Send(MdpCommon.MdpwCmd.REQUEST.ToHexString(), null, reqMsg);
                }
            }
        }

        public class Worker
        {
            //  .split worker class structure
            //  The worker class defines a single worker, idle or active:

            // Broker Instance
            public Broker Broker { get; protected set; }

            // Identity of worker as string
            public string IdString { get; protected set; }

            // Identity frame for routing
            public ZFrame Identity { get; protected set; }

            //Ownling service, if known
            public Service Service { get; set; }

            //When worker expires, if no heartbeat; 
            public DateTime Expiry { get; set; }

            /// <summary>
            /// 
            /// </summary>
            /// <param name="idString"></param>
            /// <param name="broker"></param>
            /// <param name="identity">will be dublicated inside the constructor</param>
            public Worker(string idString, Broker broker, ZFrame identity)
            {
                Broker = broker;
                IdString = idString;
                Identity = identity.Duplicate();
            }
            ~Worker()
            {
                Dispose(false);
            }

            public void Dispose()
            {
                GC.SuppressFinalize(this);
                Dispose(true);
            }

            protected void Dispose(bool disposing)
            {
                if (disposing)
                {
                    using (Identity)
                    {}
                }
            }


            public void Delete(bool disconnect)
            {
                if(disconnect)
                    Send(MdpCommon.MdpwCmd.DISCONNECT.ToHexString(), null, null);

                if (Service != null)
                {
                    Service.Waiting.Remove(this);
                    Service.Workers--;
                }

                Broker.Waiting.Remove(this);
                if (Broker.Workers.ContainsKey(IdString))
                    Broker.Workers.Remove(IdString);
            }

            //  .split worker send method
            //  This method formats and sends a command to a worker. The caller may
            //  also provide a command option, and a message payload:
            public void Send(string command, string option, ZMessage msg)
            {
                msg = msg != null
                        ? msg.Duplicate()
                        : new ZMessage();

                // Stack protocol envelope to start of message
                if (!string.IsNullOrEmpty(option))
                    msg.Prepend(new ZFrame(option));
                msg.Prepend(new ZFrame(command));
                msg.Prepend(new ZFrame(MdpCommon.MDPW_WORKER));

                // Stack routing envelope to start of message
                msg.Wrap(Identity.Duplicate());

                if(Broker.Verbose)
                    msg.DumpZmsg("I: sending '{0:X}|{0}' to worker", command.ToMdCmd());

                Broker.Socket.Send(msg);
            }

            // This worker is now waiting for work 
            public void Waiting()
            {
                // queue to broker and service waiting lists
                if (Broker == null)
                    throw new InvalidOperationException();
                Broker.Waiting.Add(this);
                Service.Waiting.Add(this);
                Expiry = DateTime.UtcNow + MdpCommon.HEARTBEAT_EXPIRY;
                Service.Dispatch(null);
            }
        }
    }

    static partial class Program
    {
        //  .split main task
        //  Finally, here is the main task. We create a new broker instance and
        //  then process messages on the broker Socket:
        public static void MDBroker(string[] args)
        {
            bool verbose = (args.Any(e => e.ToLower().Equals("-v")
                                       || e.ToLower().Equals("--verbose")));
            Console.WriteLine("Verbose: {0}", verbose);

            CancellationTokenSource cancellor = new CancellationTokenSource();
            Console.CancelKeyPress += (s, ea) =>
            {
                ea.Cancel = true;
                cancellor.Cancel();
            };

            using (Broker broker = new Broker(verbose))
            {
                broker.Bind("tcp://*:5555");
                // Get and process messages forever or until interrupted
                while (true)
                {
                    if (cancellor.IsCancellationRequested
                        || (Console.KeyAvailable && Console.ReadKey(true).Key == ConsoleKey.Escape))
                        broker.ShutdownContext();

                    var p = ZPollItem.CreateReceiver();
                    ZMessage msg;
                    ZError error;
                    if (broker.Socket.PollIn(p, out msg, out error, MdpCommon.HEARTBEAT_INTERVAL))
                    {
                        if (verbose)
                            msg.DumpZmsg("I: received message:");

                        using (ZFrame sender = msg.Pop())
                        using (ZFrame empty = msg.Pop())
                        using (ZFrame header = msg.Pop())
                        {
                            if (header.ToString().Equals(MdpCommon.MDPC_CLIENT))
                                broker.ClientMsg(sender, msg);
                            else if (header.ToString().Equals(MdpCommon.MDPW_WORKER))
                                broker.WorkerMsg(sender, msg);
                            else
                            {
                                msg.DumpZmsg("E: invalid message:");
                                msg.Dispose();
                            }
                        }
                    }
                    else
                    {
                        if (Equals(error, ZError.ETERM))
                        {
                            "W: interrupt received, shutting down...".DumpString();
                            break; // Interrupted
                        }
                        if (!Equals(error, ZError.EAGAIN))
                            throw new ZException(error);
                    }
                    // Disconnect and delete any expired workers
                    // Send heartbeats to idle workes if needed
                    if (DateTime.UtcNow > broker.HeartbeatAt)
                    {
                        broker.Purge();

                        foreach (var waitingworker in broker.Waiting)
                        {
                            waitingworker.Send(MdpCommon.MdpwCmd.HEARTBEAT.ToHexString(), null, null);
                        }
                        broker.HeartbeatAt = DateTime.UtcNow + MdpCommon.HEARTBEAT_INTERVAL;
                    }
                }
            }
        }
    }
}
