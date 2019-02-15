package guide;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZContext;

/**
* Synchronized subscriber.
*/
public class syncsub
{

    public static void main(String[] args)
    {
        try (ZContext context = new ZContext()) {
            //  First, connect our subscriber socket
            Socket subscriber = context.createSocket(SocketType.SUB);
            subscriber.connect("tcp://localhost:5561");
            subscriber.subscribe(ZMQ.SUBSCRIPTION_ALL);

            //  Second, synchronize with publisher
            Socket syncclient = context.createSocket(SocketType.REQ);
            syncclient.connect("tcp://localhost:5562");

            //  - send a synchronization request
            syncclient.send(ZMQ.MESSAGE_SEPARATOR, 0);

            //  - wait for synchronization reply
            syncclient.recv(0);

            //  Third, get our updates and report how many we got
            int update_nbr = 0;
            while (true) {
                String string = subscriber.recvStr(0);
                if (string.equals("END")) {
                    break;
                }
                update_nbr++;
            }
            System.out.println("Received " + update_nbr + " updates.");
        }
    }
}
