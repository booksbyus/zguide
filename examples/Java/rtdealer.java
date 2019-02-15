package guide;

import java.util.Random;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZContext;

/**
 * ROUTER-TO-REQ example
 */
public class rtdealer
{
    private static Random    rand        = new Random();
    private static final int NBR_WORKERS = 10;

    private static class Worker extends Thread
    {
        @Override
        public void run()
        {
            try (ZContext context = new ZContext()) {
                Socket worker = context.createSocket(SocketType.DEALER);
                ZHelper.setId(worker); //  Set a printable identity

                worker.connect("tcp://localhost:5671");

                int total = 0;
                while (true) {
                    //  Tell the broker we're ready for work
                    worker.sendMore("");
                    worker.send("Hi Boss");

                    //  Get workload from broker, until finished
                    worker.recvStr(); //  Envelope delimiter
                    String workload = worker.recvStr();
                    boolean finished = workload.equals("Fired!");
                    if (finished) {
                        System.out.printf("Completed: %d tasks\n", total);
                        break;
                    }
                    total++;

                    //  Do some random work
                    try {
                        Thread.sleep(rand.nextInt(500) + 1);
                    }
                    catch (InterruptedException e) {
                    }
                }
            }
        }
    }

    /**
     * While this example runs in a single process, that is just to make
     * it easier to start and stop the example. Each thread has its own
     * context and conceptually acts as a separate process.
     */
    public static void main(String[] args) throws Exception
    {
        try (ZContext context = new ZContext()) {
            Socket broker = context.createSocket(SocketType.ROUTER);
            broker.bind("tcp://*:5671");

            for (int workerNbr = 0; workerNbr < NBR_WORKERS; workerNbr++) {
                Thread worker = new Worker();
                worker.start();
            }

            //  Run for five seconds and then tell workers to end
            long endTime = System.currentTimeMillis() + 5000;
            int workersFired = 0;
            while (true) {
                //  Next message gives us least recently used worker
                String identity = broker.recvStr();
                broker.sendMore(identity);
                broker.recv(0); //  Envelope delimiter
                broker.recv(0); //  Response from worker
                broker.sendMore("");

                //  Encourage workers until it's time to fire them
                if (System.currentTimeMillis() < endTime)
                    broker.send("Work harder");
                else {
                    broker.send("Fired!");
                    if (++workersFired == NBR_WORKERS)
                        break;
                }
            }
        }
    }
}
