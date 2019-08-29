package guide;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ.Socket;

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

            //  Wait for signal
            receiver.recv(0);
            receiver.close();

            //  Connect to step3 and tell it we're ready
            Socket xmitter = context.createSocket(SocketType.PAIR);
            xmitter.connect("inproc://step3");
            System.out.println("Step 2 ready, signaling step 3");
            xmitter.send("READY", 0);
            xmitter.close();
        }

    }

    private static class Step3 extends Thread
    {
        private ZContext context;

        private Step3(ZContext context)
        {
            this.context = context;
        }

        @Override
        public void run()
        {
            //  Bind to inproc: endpoint, then start upstream thread
            Socket receiver = context.createSocket(SocketType.PAIR);
            receiver.bind("inproc://step3");

            //  Wait for signal
            receiver.recv(0);
            receiver.close();

            System.out.println("Step 3 ready");
        }

    }

    public static void main(String[] args) throws InterruptedException
    {
        try (ZContext context = new ZContext()) {
            //  Step 1 signals to step 2
            Thread step1 = new Step1(context);
            step1.start();

            //  Step 2 relays the signal from step 1 to step 3
            Thread step2 = new Step2(context);
            step2.start();

            //  Step 3 waits for signal from step 2
            Thread step3 = new Step3(context);
            step3.start();

            step1.join();
            step2.join();
            step3.join();

            System.out.println("Test successful!");
        }
    }
}
