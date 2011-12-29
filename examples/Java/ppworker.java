/**
 *  Paranoid Pirate worker
 *
 *  @author Arkadiusz Orzechowski <aorzecho@gmail.com>
 */
import java.util.Arrays;
import java.util.Random;

import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZMsg;

public class ppworker {

    private static final int HEARTBEAT_LIVENESS = 3; // 3-5 is reasonable
    private static final int HEARTBEAT_INTERVAL = 1000; // msecs
    private static final int INTERVAL_INIT = 1000; // Initial reconnect
    private static final int INTERVAL_MAX = 32000; // After exponential backoff

    // Paranoid Pirate Protocol constants
    private static final byte[] PPP_READY = { 1 }; // Signals worker is ready
    private static final byte[] PPP_HEARTBEAT = { 2 }; // Signals worker
                                                       // heartbeat

    private static final ZFrame heartbeatFrame = new ZFrame(PPP_HEARTBEAT);

    private static ZMQ.Socket connectWorker(ZContext context) {
        ZMQ.Socket worker = context.createSocket(ZMQ.DEALER);
        // Set random identity to make tracing easier
        Random rand = new Random();
        String id = String.format("%04x-%04x", rand.nextInt(0x10001),
                rand.nextInt(0x10001));
        worker.setIdentity(id.getBytes());
        worker.connect("tcp://localhost:5556");

        // Tell the queue we're ready for work
        System.out.printf("I: worker ready\n");
        worker.send(PPP_READY, 0);
        return worker;
    }

    /**
     * Do the job, simulate problems if cycle>5
     */
    private static boolean doTheWork(int cycle) {
        final Random rand = new Random();
        try {
            if (cycle > 3 && rand.nextInt(6) == 0) {
                System.out.printf("I: simulating a crash\n");
                return false;
            } else if (cycle > 3 && rand.nextInt(6) == 0) {
                System.out.printf("I: simulating CPU overload\n");
                Thread.sleep(3000);
            }
            System.out.printf("I: normal reply\n");
            // Do some 'work'
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        return true;
    }

    public static void main(String[] args) {
        // Prepare our context and socket
        ZContext context = new ZContext();
        ZMQ.Socket worker = connectWorker(context);

        // If liveness hits zero, queue is considered disconnected
        int liveness = HEARTBEAT_LIVENESS;
        int interval = INTERVAL_INIT;

        // Send out heartbeats at regular intervals
        long heartbeatAt = System.currentTimeMillis() + HEARTBEAT_INTERVAL;

        int cycles = 0;
        while (!Thread.currentThread().isInterrupted()) {

            ZMQ.Poller items = context.getContext().poller();
            items.register(worker, ZMQ.Poller.POLLIN);

            if (items.poll(HEARTBEAT_INTERVAL * 1000) == -1)
                break; // Interrupted

            if (items.pollin(0)) {
                ZMsg msg = ZMsg.recvMsg(worker);
                if (msg == null)
                    break; // Interrupted

                if (msg.size() == 3) { // serving a client request
                    if (!doTheWork(cycles++))
                        break; // crashed
                    liveness = HEARTBEAT_LIVENESS;
                    msg.send(worker);
                } else if (msg.size() == 1) { // heartbeat
                    ZFrame frame = msg.getFirst();
                    if (Arrays.equals(frame.getData(), PPP_HEARTBEAT)) {
                        liveness = HEARTBEAT_LIVENESS;
                    } else {
                        System.out.printf("E: invalid message (%s)\n",
                                frame.toString());
                    }
                    frame.destroy();
                } else {
                    System.out.printf("E: invalid message (%s)\n",
                            msg.toString());
                }
                interval = INTERVAL_INIT;
            } else if (--liveness == 0) {
                System.out.printf("W: heartbeat failure, can't reach queue\n");
                System.out.printf("W: reconnecting in %d msec...\n", interval);
                try {
                    Thread.sleep(interval);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                if (interval < INTERVAL_MAX)
                    interval *= 2;
                context.destroySocket(worker);
                worker = connectWorker(context);
                liveness = HEARTBEAT_LIVENESS;
            }
            // Send heartbeat to queue if it's time
            if (System.currentTimeMillis() > heartbeatAt) {
                heartbeatAt = System.currentTimeMillis() + HEARTBEAT_INTERVAL;
                System.out.printf("I: worker heartbeat\n");
                heartbeatFrame.sendAndKeep(worker);
            }
        }
        // cleanup
        context.destroy();
    }

}
