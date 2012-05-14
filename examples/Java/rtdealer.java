/**
 * Custom routing Router to Dealer.
 * Java version, based on the C version from
 * http://zguide.zeromq.org/chapter:all#toc45
 */

import org.zeromq.ZMQ;

import java.util.Arrays;
import java.util.Random;


/**
 * Router-to-dealer custom routing demo.
 *
 * The router, in this case the main function, uses ROUTER.  The
 * dealers, in this case the two worker threads, use DEALER.
 */
public class rtdealer {
    static final String URI = "ipc://routing.ipc";
    static final int NOFLAGS = 0;

    /**
     * Worker runnable consumes messages until it receives an END
     * message.
     */
    public static class Worker implements Runnable {
        public final String name;
        private final byte[] END = "END".getBytes();

        Worker(String name) { this.name = name; }

        public void run() {
            ZMQ.Context context = ZMQ.context(1);
            ZMQ.Socket socket = context.socket(ZMQ.DEALER);
            socket.setIdentity(name.getBytes());
            socket.connect(URI);

            int total = 0;
            boolean finished = false;
            while (!finished) {
                byte[] data = socket.recv(NOFLAGS);
                if (Arrays.equals(data, END)) {
                    finished = true;
                    System.out.println(
                        String.format(
                            "Worker %s received %d messages.", name, total
                        )
                    );
                }
                total += 1;
            }
            socket.close();
            context.term();
        }
    }

    /* Random number generator to determine message distribution. */
    private static Random rand = new Random();

    public static void main(String[] args)
    throws InterruptedException {
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket socket = context.socket(ZMQ.ROUTER);
        socket.bind(URI);

        Thread workerA = new Thread(new Worker("A"));
        Thread workerB = new Thread(new Worker("B"));
        workerA.start();
        workerB.start();

        // Wait a second for the workers to connect their sockets.
        System.out.println("Workers started, sleeping 1 second for warmup.");
        Thread.sleep(1000);

        // Send 10 tasks, scattered to A twice as often as B.
        for (int i = 0; i < 10; i += 1) {
            byte[] address;
            if (rand.nextInt() % 3 == 0) { // 1/3 to B.
                address = "B".getBytes();
            } else { // 2/3 to A.
                address = "A".getBytes();
            }
            socket.send(address, ZMQ.SNDMORE);
            socket.send("This is the workload.".getBytes(), NOFLAGS);
        }
        socket.send("A".getBytes(), ZMQ.SNDMORE);
        socket.send("END".getBytes(), NOFLAGS);

        socket.send("B".getBytes(), ZMQ.SNDMORE);
        socket.send("END".getBytes(), NOFLAGS);

        socket.close();
        context.term();
    }
}
