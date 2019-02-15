package guide;

import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.UUID;

import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZMsg;
import org.zeromq.ZThread;
import org.zeromq.ZThread.IAttachedRunnable;
import org.zeromq.ZThread.IDetachedRunnable;

public class titanic
{
    //  Return a new UUID as a printable character string
    //  Caller must free returned string when finished with it
    static String generateUUID()
    {
        return UUID.randomUUID().toString();
    }

    private static final String TITANIC_DIR = ".titanic";

    //  Returns freshly allocated request filename for given UUID
    private static String requestFilename(String uuid)
    {
        String filename = String.format("%s/%s.req", TITANIC_DIR, uuid);
        return filename;
    }

    //  Returns freshly allocated reply filename for given UUID
    private static String replyFilename(String uuid)
    {
        String filename = String.format("%s/%s.rep", TITANIC_DIR, uuid);
        return filename;
    }

    //  .split Titanic request service
    //  The {{titanic.request}} task waits for requests to this service. It
    //  writes each request to disk and returns a UUID to the client. The client
    //  picks up the reply asynchronously using the {{titanic.reply}} service:
    static class TitanicRequest implements IAttachedRunnable
    {
        @Override
        public void run(Object[] args, ZContext ctx, Socket pipe)
        {
            mdwrkapi worker = new mdwrkapi(
                "tcp://localhost:5555", "titanic.request", false
            );
            ZMsg reply = null;

            while (true) {
                //  Send reply if it's not null
                //  And then get next request from broker
                ZMsg request = worker.receive(reply);
                if (request == null)
                    break; //  Interrupted, exit

                //  Ensure message directory exists
                new File(TITANIC_DIR).mkdirs();

                //  Generate UUID and save message to disk
                String uuid = generateUUID();
                String filename = requestFilename(uuid);
                DataOutputStream file = null;
                try {
                    file = new DataOutputStream(new FileOutputStream(filename));
                    ZMsg.save(request, file);
                }
                catch (IOException e) {
                    e.printStackTrace();
                }
                finally {
                    try {
                        if (file != null)
                            file.close();
                    }
                    catch (IOException e) {
                    }
                }
                request.destroy();

                //  Send UUID through to message queue
                reply = new ZMsg();
                reply.add(uuid);
                reply.send(pipe);

                //  Now send UUID back to client
                //  Done by the mdwrk_recv() at the top of the loop
                reply = new ZMsg();
                reply.add("200");
                reply.add(uuid);
            }
            worker.destroy();
        }
    }

    //  .split Titanic reply service
    //  The {{titanic.reply}} task checks if there's a reply for the specified
    //  request (by UUID), and returns a 200 (OK), 300 (Pending), or 400
    //  (Unknown) accordingly:
    static class TitanicReply implements IDetachedRunnable
    {
        @Override
        public void run(Object[] args)
        {
            mdwrkapi worker = new mdwrkapi(
                "tcp://localhost:5555", "titanic.reply", false
            );
            ZMsg reply = null;

            while (true) {
                ZMsg request = worker.receive(reply);
                if (request == null)
                    break; //  Interrupted, exit

                String uuid = request.popString();
                String reqFilename = requestFilename(uuid);
                String repFilename = replyFilename(uuid);

                if (new File(repFilename).exists()) {
                    DataInputStream file = null;
                    try {
                        file = new DataInputStream(
                            new FileInputStream(repFilename)
                        );
                        reply = ZMsg.load(file);
                        reply.push("200");
                    }
                    catch (IOException e) {
                        e.printStackTrace();
                    }
                    finally {
                        try {
                            if (file != null)
                                file.close();
                        }
                        catch (IOException e) {
                        }
                    }
                }
                else {
                    reply = new ZMsg();
                    if (new File(reqFilename).exists())
                        reply.push("300"); //Pending
                    else reply.push("400"); //Unknown
                }
                request.destroy();
            }
            worker.destroy();
        }
    }

    //  .split Titanic close task
    //  The {{titanic.close}} task removes any waiting replies for the request
    //  (specified by UUID). It's idempotent, so it is safe to call more than
    //  once in a row:
    static class TitanicClose implements IDetachedRunnable
    {
        @Override
        public void run(Object[] args)
        {
            mdwrkapi worker = new mdwrkapi(
                "tcp://localhost:5555", "titanic.close", false
            );
            ZMsg reply = null;

            while (true) {
                ZMsg request = worker.receive(reply);
                if (request == null)
                    break; //  Interrupted, exit

                String uuid = request.popString();
                String req_filename = requestFilename(uuid);
                String rep_filename = replyFilename(uuid);
                new File(rep_filename).delete();
                new File(req_filename).delete();

                request.destroy();
                reply = new ZMsg();
                reply.add("200");
            }
            worker.destroy();
        }
    }

    //  .split worker task
    //  This is the main thread for the Titanic worker. It starts three child
    //  threads; for the request, reply, and close services. It then dispatches
    //  requests to workers using a simple brute force disk queue. It receives
    //  request UUIDs from the {{titanic.request}} service, saves these to a
    //  disk file, and then throws each request at MDP workers until it gets a
    //  response.
    public static void main(String[] args)
    {
        boolean verbose = (args.length > 0 && "-v".equals(args[0]));

        try (ZContext ctx = new ZContext()) {
            Socket requestPipe = ZThread.fork(ctx, new TitanicRequest());
            ZThread.start(new TitanicReply());
            ZThread.start(new TitanicClose());

            Poller poller = ctx.createPoller(1);
            poller.register(requestPipe, ZMQ.Poller.POLLIN);

            //  Main dispatcher loop
            while (true) {
                //  We'll dispatch once per second, if there's no activity
                int rc = poller.poll(1000);
                if (rc == -1)
                    break; //  Interrupted
                if (poller.pollin(0)) {
                    //  Ensure message directory exists
                    new File(TITANIC_DIR).mkdirs();

                    //  Append UUID to queue, prefixed with '-' for pending
                    ZMsg msg = ZMsg.recvMsg(requestPipe);
                    if (msg == null)
                        break; //  Interrupted
                    String uuid = msg.popString();
                    BufferedWriter wfile = null;
                    try {
                        wfile = new BufferedWriter(
                            new FileWriter(TITANIC_DIR + "/queue", true)
                        );
                        wfile.write("-" + uuid + "\n");
                    }
                    catch (IOException e) {
                        e.printStackTrace();
                        break;
                    }
                    finally {
                        try {
                            if (wfile != null)
                                wfile.close();
                        }
                        catch (IOException e) {
                        }
                    }
                    msg.destroy();
                }

                // Brute force dispatcher
                // "?........:....:....:....:............:";
                byte[] entry = new byte[37];

                RandomAccessFile file = null;

                try {
                    file = new RandomAccessFile(TITANIC_DIR + "/queue", "rw");
                    while (file.read(entry) > 0) {
                        //  UUID is prefixed with '-' if still waiting
                        if (entry[0] == '-') {
                            if (verbose)
                                System.out.printf(
                                    "I: processing request %s\n",
                                    new String(
                                        entry, 1, entry.length - 1, ZMQ.CHARSET
                                    )
                                );
                            if (serviceSuccess(
                                    new String(
                                        entry, 1, entry.length - 1, ZMQ.CHARSET
                                    )
                                )) {
                                //  Mark queue entry as processed
                                file.seek(file.getFilePointer() - 37);
                                file.writeBytes("+");
                                file.seek(file.getFilePointer() + 36);
                            }
                        }

                        //  Skip end of line, LF or CRLF
                        if (file.readByte() == '\r')
                            file.readByte();

                        if (Thread.currentThread().isInterrupted())
                            break;
                    }
                }
                catch (FileNotFoundException e) {
                }
                catch (IOException e) {
                    e.printStackTrace();
                }
                finally {
                    if (file != null) {
                        try {
                            file.close();
                        }
                        catch (IOException e) {
                        }
                    }
                }
            }
        }
    }

    //  .split try to call a service
    //  Here, we first check if the requested MDP service is defined or not,
    //  using a MMI lookup to the Majordomo broker. If the service exists, we
    //  send a request and wait for a reply using the conventional MDP client
    //  API. This is not meant to be fast, just very simple:
    static boolean serviceSuccess(String uuid)
    {
        //  Load request message, service will be first frame
        String filename = requestFilename(uuid);

        //  If the client already closed request, treat as successful
        if (!new File(filename).exists())
            return true;

        DataInputStream file = null;
        ZMsg request;
        try {
            file = new DataInputStream(new FileInputStream(filename));
            request = ZMsg.load(file);
        }
        catch (IOException e) {
            e.printStackTrace();
            return true;
        }
        finally {
            try {
                if (file != null)
                    file.close();
            }
            catch (IOException e) {
            }
        }
        ZFrame service = request.pop();
        String serviceName = service.toString();

        //  Create MDP client session with short timeout
        mdcliapi client = new mdcliapi("tcp://localhost:5555", false);
        client.setTimeout(1000); //  1 sec
        client.setRetries(1); //  only 1 retry

        //  Use MMI protocol to check if service is available
        ZMsg mmiRequest = new ZMsg();
        mmiRequest.add(service);
        ZMsg mmiReply = client.send("mmi.service", mmiRequest);
        boolean serviceOK = (mmiReply != null &&
                             mmiReply.getFirst().toString().equals("200"));
        mmiReply.destroy();

        boolean result = false;
        if (serviceOK) {
            ZMsg reply = client.send(serviceName, request);
            if (reply != null) {
                filename = replyFilename(uuid);
                DataOutputStream ofile = null;
                try {
                    ofile = new DataOutputStream(new FileOutputStream(filename));
                    ZMsg.save(reply, ofile);
                }
                catch (IOException e) {
                    e.printStackTrace();
                    return true;
                }
                finally {
                    try {
                        if (file != null)
                            file.close();
                    }
                    catch (IOException e) {
                    }
                }
                result = true;
            }
            reply.destroy();
        }
        else request.destroy();

        client.destroy();
        return result;
    }
}
