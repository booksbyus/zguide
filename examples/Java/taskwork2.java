package guide;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

/**
 *  Task worker - design 2
 *  Adds pub-sub flow to receive and respond to kill signal
 */
public class taskwork2
{
    public static void main(String[] args) throws InterruptedException
    {
        try (ZContext context = new ZContext()) {
            ZMQ.Socket receiver = context.createSocket(SocketType.PULL);
            receiver.connect("tcp://localhost:5557");

            ZMQ.Socket sender = context.createSocket(SocketType.PUSH);
            sender.connect("tcp://localhost:5558");

            ZMQ.Socket controller = context.createSocket(SocketType.SUB);
            controller.connect("tcp://localhost:5559");
            controller.subscribe(ZMQ.SUBSCRIPTION_ALL);

            ZMQ.Poller items = context.createPoller(2);
            items.register(receiver, ZMQ.Poller.POLLIN);
            items.register(controller, ZMQ.Poller.POLLIN);

            while (true) {
                items.poll();

                if (items.pollin(0)) {

                    String message = receiver.recvStr(0);
                    long nsec = Long.parseLong(message);

                    //  Simple progress indicator for the viewer
                    System.out.print(message + '.');
                    System.out.flush();

                    //  Do the work
                    Thread.sleep(nsec);

                    //  Send results to sink
                    sender.send("", 0);
                }

                //  Any waiting controller command acts as 'KILL'
                if (items.pollin(1)) {
                    break; // Exit loop
                }
            }
        }
    }
}
