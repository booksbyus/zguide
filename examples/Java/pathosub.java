package guide;

import java.util.Random;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;

//  Pathological subscriber
//  Subscribes to one random topic and prints received messages
public class pathosub
{
    public static void main(String[] args)
    {
        try (ZContext context = new ZContext()) {
            Socket subscriber = context.createSocket(SocketType.SUB);
            if (args.length == 1)
                subscriber.connect(args[0]);
            else subscriber.connect("tcp://localhost:5556");

            Random rand = new Random(System.currentTimeMillis());
            String subscription = String.format("%03d", rand.nextInt(1000));
            subscriber.subscribe(subscription.getBytes(ZMQ.CHARSET));

            while (true) {
                String topic = subscriber.recvStr();
                if (topic == null)
                    break;
                String data = subscriber.recvStr();
                assert (topic.equals(subscription));
                System.out.println(data);
            }
        }
    }
}
