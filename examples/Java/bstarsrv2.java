/*
 *     bstarsrv2.java
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
import org.zeromq.ZLoop;
import org.zeromq.ZLoop.IZLoopHandler;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.PollItem;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZMsg;

//  Binary Star server, using bstar reactor
public class bstarsrv2
{
    private static IZLoopHandler Echo = new IZLoopHandler()
    {
        @Override
        public int handle(ZLoop loop, PollItem item, Object arg)
        {
            ZMsg msg = ZMsg.recvMsg(item.getSocket());
            msg.send(item.getSocket());
            return 0;
        }
    };

    public static void main(String[] argv) {
        //  Arguments can be either of:
        //      -p  primary server, at tcp://localhost:5001
        //      -b  backup server, at tcp://localhost:5002
        bstar bs = null;

        if (argv.length == 1 && argv[0].equals("-p")) {
            System.out.printf("I: Primary active, waiting for backup (passive)\n");
            bs = new bstar(true, "tcp://*:5003", "tcp://localhost:5004");
            bs.voter ("tcp://*:5001", ZMQ.ROUTER, Echo, null);
        }
        else
        if (argv.length == 1 && argv[0].equals("-b")) {
            System.out.printf("I: Backup passive, waiting for primary (active)\n");
            bs = new bstar(false, "tcp://*:5004", "tcp://localhost:5003");
            bs.voter ("tcp://*:5002", ZMQ.ROUTER, Echo, null);
        }
        else {
            System.out.printf("Usage: bstarsrv { -p | -b }\n");
            System.exit(0);
        }
        bs.start();
        bs.destroy();
    }
}
