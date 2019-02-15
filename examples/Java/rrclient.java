package guide;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZContext;

/**
* Hello World client
* Connects REQ socket to tcp://localhost:5559
* Sends "Hello" to server, expects "World" back
*/
public class rrclient
{

    public static void main(String[] args)
    {
        try (ZContext context = new ZContext()) {
            //  Socket to talk to server
            Socket requester = context.createSocket(SocketType.REQ);
            requester.connect("tcp://localhost:5559");

            System.out.println("launch and connect client.");

            for (int request_nbr = 0; request_nbr < 10; request_nbr++) {
                requester.send("Hello", 0);
                String reply = requester.recvStr(0);
                System.out.println(
                    "Received reply " + request_nbr + " [" + reply + "]"
                );
            }
        }
    }
}
