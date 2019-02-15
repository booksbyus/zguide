package guide;

import java.util.Random;

//  Suicidal Snail

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZThread;
import org.zeromq.ZThread.IAttachedRunnable;

public class suisnail
{
    private static final long MAX_ALLOWED_DELAY = 1000; //  msecs
    private static Random rand = new Random(System.currentTimeMillis());

    //  This is our subscriber. It connects to the publisher and subscribes to
    //  everything. It sleeps for a short time between messages to simulate
    //  doing too much work. If a message is more than one second late, it
    //  croaks.
    private static class Subscriber implements IAttachedRunnable
    {
        @Override
        public void run(Object[] args, ZContext ctx, Socket pipe)
        {
            //  Subscribe to everything
            Socket subscriber = ctx.createSocket(SocketType.SUB);
            subscriber.subscribe(ZMQ.SUBSCRIPTION_ALL);
            subscriber.connect("tcp://localhost:5556");

            //  Get and process messages
            while (true) {
                String string = subscriber.recvStr();
                System.out.printf("%s\n", string);
                long clock = Long.parseLong(string);

                //  Suicide snail logic
                if (System.currentTimeMillis() - clock > MAX_ALLOWED_DELAY) {
                    System.err.println(
                        "E: subscriber cannot keep up, aborting"
                    );
                    break;
                }
                //  Work for 1 msec plus some random additional time
                try {
                    Thread.sleep(1000 + rand.nextInt(2000));
                }
                catch (InterruptedException e) {
                    break;
                }
            }
            pipe.send("gone and died");
        }
    }

    //  .split publisher task
    //  This is our publisher task. It publishes a time-stamped message to its
    //  PUB socket every millisecond:
    private static class Publisher implements IAttachedRunnable
    {
        @Override
        public void run(Object[] args, ZContext ctx, Socket pipe)
        {
            //  Prepare publisher
            Socket publisher = ctx.createSocket(SocketType.PUB);
            publisher.bind("tcp://*:5556");

            while (true) {
                //  Send current clock (msecs) to subscribers
                String string = String.format("%d", System.currentTimeMillis());
                publisher.send(string);
                String signal = pipe.recvStr(ZMQ.DONTWAIT);
                if (signal != null) {
                    break;
                }
                try {
                    Thread.sleep(1);
                }
                catch (InterruptedException e) {
                }
            }
        }
    }

    //  .split main task
    //  The main task simply starts a client and a server, and then waits for
    //  the client to signal that it has died:
    public static void main(String[] args) throws Exception
    {
        try (ZContext ctx = new ZContext()) {
            Socket pubpipe = ZThread.fork(ctx, new Publisher());
            Socket subpipe = ZThread.fork(ctx, new Subscriber());
            subpipe.recvStr();
            pubpipe.send("break");
            Thread.sleep(100);
        }
    }
}
