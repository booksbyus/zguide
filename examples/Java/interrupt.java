package guide;

/*
*
*  Interrupt in Java
*  Shows how to handle Ctrl-C
*/

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZMQException;
import org.zeromq.ZContext;

public class interrupt
{
    public static void main(String[] args)
    {
        //  Prepare our context and socket
        final ZContext context = new ZContext();

        final Thread zmqThread = new Thread()
        {
            @Override
            public void run()
            {
                ZMQ.Socket socket = context.createSocket(SocketType.REP);
                socket.bind("tcp://*:5555");

                while (!Thread.currentThread().isInterrupted()) {
                    try {
                        socket.recv(0);
                    }
                    catch (ZMQException e) {
                        if (e.getErrorCode() == ZMQ.Error.ETERM.getCode()) {
                            break;
                        }
                    }
                }

                socket.setLinger(0);
                socket.close();
            }
        };

        Runtime.getRuntime().addShutdownHook(new Thread()
        {
            @Override
            public void run()
            {
                System.out.println("W: interrupt received, killing server...");
                context.close();
                try {
                    zmqThread.interrupt();
                    zmqThread.join();
                }
                catch (InterruptedException e) {
                }
            }
        });

        zmqThread.start();
    }
}
