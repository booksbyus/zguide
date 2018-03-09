import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;

/**
 * Multi threaded Hello World server
 */
public class mtserver {

    private static class Worker extends Thread
    {
        private Context context;

        private Worker (Context context)
        {
            this.context = context;
        }
        @Override
        public void run() {
            ZMQ.Socket socket = context.socket(ZMQ.REP);
            socket.connect ("inproc://workers");

            while (true) {

                //  Wait for next request from client (C string)
                String request = socket.recvStr (0);
                System.out.println ( Thread.currentThread().getName() + " Received request: [" + request + "]");

                //  Do some 'work'
                try {
                    Thread.sleep (1000);
                } catch (InterruptedException e) {
                }

                //  Send reply back to client (C string)
                socket.send("world", 0);
            }
        }
    }
    
    public static void main (String[] args) {

        Context context = ZMQ.context(1);

        Socket clients = context.socket(ZMQ.ROUTER);
        clients.bind ("tcp://*:5555");

        Socket workers = context.socket(ZMQ.DEALER);
        workers.bind ("inproc://workers");

        for(int thread_nbr = 0; thread_nbr < 5; thread_nbr++) {
            Thread worker = new Worker (context);
            worker.start();
        }
        //  Connect work threads to client threads via a queue
        ZMQ.proxy (clients, workers, null);

        //  We never get here but clean up anyhow
        clients.close();
        workers.close();
        context.term();
    }
}
