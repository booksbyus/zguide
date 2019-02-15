package guide;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZContext;

/**
 * Multithreaded relay
 */
public class mtrelay
{

    private static class Step1 extends Thread
    {
        private ZContext context;

        private Step1(ZContext context)
        {
            this.context = context;
        }

        @Override
        public void run()
        {
            //  Signal downstream to step 2
            Socket xmitter = context.createSocket(SocketType.PAIR);
            xmitter.connect("inproc://step2");
            System.out.println("Step 1 ready, signaling step 2");
            xmitter.send("READY", 0);
            xmitter.close();
        }

    }

    private static class Step2 extends Thread
    {
        private ZContext context;

        private Step2(ZContext context)
        {
            this.context = context;
        }

        @Override
        public void run()
        {
            //  Bind to inproc: endpoint, then start upstream thread
            Socket receiver = context.createSocket(SocketType.PAIR);
            receiver.bind("inproc://step2");
            Thread step1 = new Step1(context);
            step1.start();

            //  Wait for signal
            receiver.recv(0);
            receiver.close();

            //  Connect to step3 and tell it we're ready
            Socket xmitter = context.createSocket(SocketType.PAIR);
            xmitter.connect("inproc://step3");
            xmitter.send("READY", 0);

            xmitter.close();
        }

    }

    public static void main(String[] args)
    {
        try (ZContext context = new ZContext()) {
            //  Bind to inproc: endpoint, then start upstream thread
            Socket receiver = context.createSocket(SocketType.PAIR);
            receiver.bind("inproc://step3");

            //  Step 2 relays the signal to step 3
            Thread step2 = new Step2(context);
            step2.start();

            //  Wait for signal
            receiver.recv(0);
            receiver.close();

            System.out.println("Test successful!");
        }
    }
}
