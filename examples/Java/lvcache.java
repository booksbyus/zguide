package guide;

import java.util.HashMap;
import java.util.Map;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;

//  Last value cache
//  Uses XPUB subscription messages to re-send data
public class lvcache
{
    public static void main(String[] args)
    {
        try (ZContext context = new ZContext()) {
            Socket frontend = context.createSocket(SocketType.SUB);
            frontend.bind("tcp://*:5557");
            Socket backend = context.createSocket(SocketType.XPUB);
            backend.bind("tcp://*:5558");

            //  Subscribe to every single topic from publisher
            frontend.subscribe(ZMQ.SUBSCRIPTION_ALL);

            //  Store last instance of each topic in a cache
            Map<String, String> cache = new HashMap<String, String>();

            Poller poller = context.createPoller(2);
            poller.register(frontend, Poller.POLLIN);
            poller.register(backend, Poller.POLLIN);

            //  .split main poll loop
            //  We route topic updates from frontend to backend, and we handle
            //  subscriptions by sending whatever we cached, if anything:
            while (true) {
                if (poller.poll(1000) == -1)
                    break; //  Interrupted

                //  Any new topic data we cache and then forward
                if (poller.pollin(0)) {
                    String topic = frontend.recvStr();
                    String current = frontend.recvStr();

                    if (topic == null)
                        break;
                    cache.put(topic, current);
                    backend.sendMore(topic);
                    backend.send(current);
                }
                //  .split handle subscriptions
                //  When we get a new subscription, we pull data from the cache:
                if (poller.pollin(1)) {
                    ZFrame frame = ZFrame.recvFrame(backend);
                    if (frame == null)
                        break;
                    //  Event is one byte 0=unsub or 1=sub, followed by topic
                    byte[] event = frame.getData();
                    if (event[0] == 1) {
                        String topic = new String(event, 1, event.length - 1, ZMQ.CHARSET);
                        System.out.printf("Sending cached topic %s\n", topic);
                        String previous = cache.get(topic);
                        if (previous != null) {
                            backend.sendMore(topic);
                            backend.send(previous);
                        }
                    }
                    frame.destroy();
                }
            }
        }
    }
}
