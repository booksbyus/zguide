package guide;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

//
//  Reading from multiple sockets in Java
//  This version uses a simple recv loop
//
public class msreader
{

    public static void main(String[] args) throws Exception
    {
        //  Prepare our context and sockets
        try (ZContext context = new ZContext()) {
            // Connect to task ventilator
            ZMQ.Socket receiver = context.createSocket(SocketType.PULL);
            receiver.connect("tcp://localhost:5557");

            //  Connect to weather server
            ZMQ.Socket subscriber = context.createSocket(SocketType.SUB);
            subscriber.connect("tcp://localhost:5556");
            subscriber.subscribe("10001 ".getBytes(ZMQ.CHARSET));

            //  Process messages from both sockets
            //  We prioritize traffic from the task ventilator
            while (!Thread.currentThread().isInterrupted()) {
                //  Process any waiting tasks
                byte[] task;
                while ((task = receiver.recv(ZMQ.DONTWAIT)) != null) {
                    System.out.println("process task");
                }
                //  Process any waiting weather updates
                byte[] update;
                while ((update = subscriber.recv(ZMQ.DONTWAIT)) != null) {
                    System.out.println("process weather update");
                }
                //  No activity, so sleep for 1 msec
                Thread.sleep(1000);
            }
        }
    }
}
