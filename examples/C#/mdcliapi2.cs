using System;
using System.Threading;

using ZeroMQ;

namespace Examples
{
    namespace MDCliApi2
    {
        //
        //  mdcliapi class - Majordomo Protocol Client API
        //  Implements the MDP/Worker spec at http://rfc.zeromq.org/spec:7.
        //
        // Author: metadings
        //

        public class MajordomoClient : IDisposable
        {
            //  Structure of our class
            //  We access these properties only via class methods

            // Our context
            readonly ZContext _context;

            // Majordomo broker
            public string Broker { get; protected set; }

            //  Socket to broker
            public ZSocket Client { get; protected set; }

            //  Print activity to console
            public bool Verbose { get; protected set; }

            //  Request timeout
            public TimeSpan Timeout { get; protected set; }

            public void ConnectToBroker()
            {
                //  Connect or reconnect to broker. In this asynchronous class we use a
                //  DEALER socket instead of a REQ socket; this lets us send any number
                //  of requests without waiting for a reply.

                Client = new ZSocket(_context, ZSocketType.DEALER);
                Client.Connect(Broker);
                if (Verbose)
                    "I: connecting to broker at '{0}'...".DumpString(Broker);
                
            }

            //  The constructor and destructor are the same as in mdcliapi, except
            //  we don't do retries, so there's no retries property.
            //  .skip
            //  ---------------------------------------------------------------------
            public MajordomoClient(string broker, bool verbose)
            {
                if(broker == null)
                    throw new InvalidOperationException();
                _context = new ZContext();
                Broker = broker;
                Verbose = verbose;
                Timeout = TimeSpan.FromMilliseconds(2500);

                ConnectToBroker();
            }

            ~MajordomoClient()
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

                    if (Client != null)
                    {
                        Client.Dispose();
                        Client = null;
                    }
                    //Do not Dispose Context: cuz of weird shutdown behavior, stucks in using calls 
                }
            }

            //  Set request timeout
            public void Set_Timeout(int timeoutInMs)
            {
                Timeout = TimeSpan.FromMilliseconds(timeoutInMs);
            }

            //  .until
            //  .skip
            //  The send method now just sends one message, without waiting for a
            //  reply. Since we're using a DEALER socket we have to send an empty
            //  frame at the start, to create the same envelope that the REQ socket
            //  would normally make for us:

            public int Send(string service, ZMessage request, CancellationTokenSource cancellor)
            {
                if (request == null)
                    throw new NotImplementedException();

                if (cancellor.IsCancellationRequested
                        || (Console.KeyAvailable && Console.ReadKey(true).Key == ConsoleKey.Escape))
                    _context.Shutdown();

                //  Prefix request with protocol frames
                //  Frame 0: empty (REQ emulation)
                //  Frame 1: "MDPCxy" (six bytes, MDP/Client x.y)
                //  Frame 2: Service name (printable string)
                request.Prepend(new ZFrame(service));
                request.Prepend(new ZFrame(MdpCommon.MDPC_CLIENT));
                request.Prepend(new ZFrame(string.Empty));

                if (Verbose)
                    request.DumpZmsg("I: send request to '{0}' service:", service);

                ZError error; 
                if(!Client.Send(request, out error));
                {
                    if (Equals(error, ZError.ETERM))
                        cancellor.Cancel(); // Interrupted
                    //throw new ZException(error);
                }
                return 0;
            }

            //  .skip
            //  The recv method waits for a reply message and returns that to the 
            //  caller.
            //  ---------------------------------------------------------------------
            //  Returns the reply message or NULL if there was no reply. Does not
            //  attempt to recover from a broker failure, this is not possible
            //  without storing all unanswered requests and resending them all...
            public ZMessage Recv(CancellationTokenSource cancellor)
            {
                //  Poll socket for a reply, with timeout
                var p = ZPollItem.CreateReceiver();
                ZMessage msg;
                ZError error;
                //  .split body of send 
                //  On any blocking call, {{libzmq}} will return -1 if there was
                //  an error; we could in theory check for different error codes,
                //  but in practice it's OK to assume it was {{EINTR}} (Ctrl-C):

                // Poll the client Message
                if (Client.PollIn(p, out msg, out error, Timeout))
                {
                    //  If we got a reply, process it
                    if (Verbose)
                        msg.DumpZmsg("I: received reply");

                    //  Don't try to handle errors, just assert noisily
                    if (msg.Count < 4)
                        throw new InvalidOperationException();

                    using (ZFrame empty = msg.Pop())
                        if (!empty.ToString().Equals(string.Empty))
                            throw new InvalidOperationException();

                    using (ZFrame header = msg.Pop())
                        if (!header.ToString().Equals(MdpCommon.MDPC_CLIENT))
                            throw new InvalidOperationException();

                    using (ZFrame replyService = msg.Pop())
                    {}

                    return msg;
                }
                else if (Equals(error, ZError.ETERM))
                {
                    "W: interrupt received, killing client...\n".DumpString();
                    cancellor.Cancel();
                }
                else 
                {
                    if (Verbose)
                        "W: permanent error, abandoning Error: {0}".DumpString(error);
                }

                return null;
            }

        }
    }
}
