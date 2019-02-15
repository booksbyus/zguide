package guide;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

//
//  Task worker in Java
//  Connects PULL socket to tcp://localhost:5557
//  Collects workloads from ventilator via that socket
//  Connects PUSH socket to tcp://localhost:5558
//  Sends results to sink via that socket
//
public class taskwork
{
    public static void main(String[] args) throws Exception
    {
        try (ZContext context = new ZContext()) {
            //  Socket to receive messages on
            ZMQ.Socket receiver = context.createSocket(SocketType.PULL);
            receiver.connect("tcp://localhost:5557");

            //  Socket to send messages to
            ZMQ.Socket sender = context.createSocket(SocketType.PUSH);
            sender.connect("tcp://localhost:5558");

            //  Process tasks forever
            while (!Thread.currentThread().isInterrupted()) {
                String string = new String(receiver.recv(0), ZMQ.CHARSET).trim();
                long msec = Long.parseLong(string);
                //  Simple progress indicator for the viewer
                System.out.flush();
                System.out.print(string + '.');

                //  Do the work
                Thread.sleep(msec);

                //  Send results to sink
                sender.send(ZMQ.MESSAGE_SEPARATOR, 0);
            }
        }
    }
}
