/*
 *     bstarsrv2.java
 *
 */


import org.zeromq.ZLoop;
import org.zeromq.ZLoop.IZLoopHandler;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.PollItem;
import org.zeromq.ZMsg;

//  Binary Star server, using Bstar reactor
public class Bstarsrv2
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
        Bstar bs = null;

        if (argv.length == 1 && argv[0].equals("-p")) {
            System.out.printf("I: Primary active, waiting for backup (passive)\n");
            bs = new Bstar(true, "tcp://*:5003", "tcp://localhost:5004");
            bs.voter ("tcp://*:5001", ZMQ.ROUTER, Echo, null);
        }
        else
        if (argv.length == 1 && argv[0].equals("-b")) {
            System.out.printf("I: Backup passive, waiting for primary (active)\n");
            bs = new Bstar(false, "tcp://*:5004", "tcp://localhost:5003");
            bs.voter ("tcp://*:5002", ZMQ.ROUTER, Echo, null);
        }
        else {
            System.out.printf("Usage: Bstarsrv { -p | -b }\n");
            System.exit(0);
        }
        bs.start();
        bs.destroy();
    }
}
