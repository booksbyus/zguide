package guide;

import java.nio.ByteBuffer;
import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZContext;

/**
 * 
 * Clone server model 1
 * @author Danish Shrestha <dshrestha06@gmail.com>
 *
 */
public class clonesrv1
{
    private static AtomicLong sequence = new AtomicLong();

    public void run()
    {
        try (ZContext ctx = new ZContext()) {
            Socket publisher = ctx.createSocket(SocketType.PUB);
            publisher.bind("tcp://*:5556");

            try {
                Thread.sleep(200);
            }
            catch (InterruptedException e) {
                e.printStackTrace();
            }

            Random random = new Random();

            while (true) {
                long currentSequenceNumber = sequence.incrementAndGet();
                int key = random.nextInt(10000);
                int body = random.nextInt(1000000);

                ByteBuffer b = ByteBuffer.allocate(4);
                b.asIntBuffer().put(body);

                kvsimple kvMsg = new kvsimple(
                    key + "", currentSequenceNumber, b.array()
                );
                kvMsg.send(publisher);
                System.out.println("sending " + kvMsg);
            }
        }
    }

    public static void main(String[] args)
    {
        new clonesrv1().run();
    }
}
