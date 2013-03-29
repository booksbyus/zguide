/*
 *     flclient2.java
 *
 *     -------------------------------------------------------------------------
 *     Copyright (c) 2012-2013 InfiniLoop Corporation
 *     Copyright other contributors as noted in the AUTHORS file.
 *
 *     This file is part of Zyni, an open-source message based application framework.
 *
 *     This is free software; you can redistribute it and/or modify it under
 *     the terms of the GNU Lesser General Public License as published by the
 *     Free Software Foundation; either version 3 of the License, or (at your
 *     option) any later version.
 *
 *     This software is distributed in the hope that it will be useful, but
 *     WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTA-
 *     BILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General
 *     Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with this program. If not, see http://www.gnu.org/licenses/.
 *     =========================================================================
 */


import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.PollItem;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZMsg;

//  Freelance client - Model 2
//  Uses DEALER socket to blast one or more services
public class flclient2
{
    //  If not a single service replies within this time, give up
    private static final int GLOBAL_TIMEOUT = 2500;

    //  .split class implementation
    //  Here is the {{flclient}} class implementation. Each instance has a
    //  context, a DEALER socket it uses to talk to the servers, a counter
    //  of how many servers it's connected to, and a request sequence number:
    private ZContext ctx;        //  Our context wrapper
    private Socket socket;       //  DEALER socket talking to servers
    private int servers;         //  How many servers we have connected to
    private int sequence;        //  Number of requests ever sent

    public flclient2()
    {
        ctx = new ZContext();
        socket = ctx.createSocket(ZMQ.DEALER);
    }

    public void destroy()
    {
        ctx.destroy();
    }

    private void connect(String endpoint)
    {
        socket.connect(endpoint);
        servers++;
    }

    private ZMsg request(ZMsg request)
    {
        //  Prefix request with sequence number and empty envelope
        String sequenceText = String.format("%d", ++sequence);
        request.push(sequenceText);
        request.push("");

        //  Blast the request to all connected servers
        int server;
        for (server = 0; server < servers; server++) {
            ZMsg msg = request.duplicate();
            msg.send(socket);
        }
        //  Wait for a matching reply to arrive from anywhere
        //  Since we can poll several times, calculate each one
        ZMsg reply = null;
        long endtime = System.currentTimeMillis() + GLOBAL_TIMEOUT;
        while (System.currentTimeMillis() < endtime) {
            PollItem[] items = { new PollItem(socket, ZMQ.Poller.POLLIN) };
            ZMQ.poll(items, endtime - System.currentTimeMillis());
            if (items[0].isReadable()) {
                //  Reply is [empty][sequence][OK]
                reply = ZMsg.recvMsg(socket);
                assert (reply.size() == 3);
                reply.pop();
                String sequenceStr = reply.popString();
                int sequenceNbr = Integer.parseInt(sequenceStr);
                if (sequenceNbr == sequence)
                    break;
                reply.destroy();
            }
        }
        request.destroy();
        return reply;

    }

    public static void main (String[] argv)
    {
        if (argv.length == 0) {
            System.out.printf ("I: syntax: flclient2 <endpoint> ...\n");
            System.exit(0);
        }

        //  Create new freelance client object
        flclient2 client = new flclient2();

        //  Connect to each endpoint
        int argn;
        for (argn = 0; argn < argv.length; argn++)
            client.connect(argv[argn]);

        //  Send a bunch of name resolution 'requests', measure time
        int requests = 10000;
        long start = System.currentTimeMillis();
        while (requests-- > 0) {
            ZMsg request = new ZMsg();
            request.add("random name");
            ZMsg reply = client.request(request);
            if (reply == null) {
                System.out.printf("E: name service not available, aborting\n");
                break;
            }
            reply.destroy();
        }
        System.out.printf ("Average round trip cost: %d usec\n",
                (int) (System.currentTimeMillis() - start) / 10);

        client.destroy();
    }

}
