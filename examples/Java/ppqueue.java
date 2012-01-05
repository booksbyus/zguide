/**
 *  Paranoid Pirate queue
 *
 *  @author Arkadiusz Orzechowski <aorzecho@gmail.com>
 */
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Deque;

import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZMsg;

public class ppqueue {

    private static final int HEARTBEAT_LIVENESS = 3; // 3-5 is reasonable
    private static final int HEARTBEAT_INTERVAL = 1000; // msecs

    private static final byte[] PPP_READY = { 1 }; // Signals worker is ready
    private static final byte[] PPP_HEARTBEAT = { 2 }; // Signals worker
                                                       // heartbeat

    /**
     * Keeps worker's address and expiry time.
     */
    private static class Worker {
        ZFrame address;
        long expiry;

        public Worker(ZFrame address) {
            this.address = address;
            this.expiry = System.currentTimeMillis() + HEARTBEAT_INTERVAL
                    * HEARTBEAT_LIVENESS;
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(address.getData());
        }

        @Override
        public boolean equals(Object obj) {
            if (!(obj instanceof Worker))
                return false;
            Worker other = (Worker) obj;
            return Arrays.equals(address.getData(), other.address.getData());
        }

    }

    private static class WorkersPool {
        private Deque<Worker> workers = new ArrayDeque<Worker>();
        private static final ZFrame heartbeatFrame = new ZFrame(PPP_HEARTBEAT);
        private long heartbeatAt = System.currentTimeMillis()
                + HEARTBEAT_INTERVAL;

        /**
         * Worker is ready, remove if on list and move to end
         */
        public synchronized void workerReady(Worker worker) {
            if (workers.remove(worker)) {
                System.out.printf("I:    %s is alive, waiting\n",
                        worker.address.toString());
            } else {
                System.out.printf("I: %s is now ready to work\n",
                        worker.address.toString());
            }
            workers.offerLast(worker);
        }

        /**
         * Return next available worker address
         */
        public synchronized ZFrame next() {
            return workers.pollFirst().address;
        }

        /**
         * Send heartbeats to idle workers if it's time
         */
        public synchronized void sendHeartbeats(Socket backend) {
            // Send heartbeats to idle workers if it's time
            if (System.currentTimeMillis() >= heartbeatAt) {
                for (Worker worker : workers) {
                    worker.address.sendAndKeep(backend, ZMQ.SNDMORE);
                    heartbeatFrame.sendAndKeep(backend);
                }
                heartbeatAt = System.currentTimeMillis() + HEARTBEAT_INTERVAL;
            }
        }

        /**
         * Look for & kill expired workers. Workers are oldest to most recent,
         * so we stop at the first alive worker.
         */
        public synchronized void purge() {
            for (Worker w = workers.peekFirst(); w != null
                    && w.expiry < System.currentTimeMillis(); w = workers
                    .peekFirst()) {
                workers.pollFirst().address.destroy();
            }
        }

        public boolean isEmpty() {
            return workers.isEmpty();
        }

        public synchronized void close() {
            for (Worker worker : workers)
                worker.address.destroy();
        }
    }

    public static void main(String[] args) {
        // Prepare our context and sockets
        ZContext context = new ZContext();
        ZMQ.Socket frontend = context.createSocket(ZMQ.ROUTER);
        ZMQ.Socket backend = context.createSocket(ZMQ.ROUTER);
        frontend.bind("tcp://*:5555"); // For clients
        backend.bind("tcp://*:5556"); // For workers
        WorkersPool workers = new WorkersPool();

        while (!Thread.currentThread().isInterrupted()) {
            ZMQ.Poller items = context.getContext().poller();
            items.register(backend, ZMQ.Poller.POLLIN);

            if (!workers.isEmpty()) // poll frontend only if there are
                                    // registered workers
                items.register(frontend, ZMQ.Poller.POLLIN);

            items.poll();
            if (items.pollin(0)) {
                // receive whole message (all ZFrames) at once
                ZMsg msg = ZMsg.recvMsg(backend);
                if (msg == null)
                    break; // Interrupted

                // Any sign of life from worker means it's ready
                ZFrame address = msg.unwrap();
                workers.workerReady(new Worker(address));

                // Validate control message, or return reply to client
                if (msg.size() == 1) {
                    ZFrame frame = msg.getFirst();
                    if (!(Arrays.equals(frame.getData(), PPP_HEARTBEAT) || Arrays
                            .equals(frame.getData(), PPP_READY))) {
                        System.out.printf("E: invalid message from worker "
                                + msg.toString());
                    }
                    msg.destroy();
                } else
                    msg.send(frontend);
            }
            if (items.pollin(1)) {
                // Now get next client request, route to next worker
                ZMsg msg = ZMsg.recvMsg(frontend);
                if (msg == null)
                    break; // Interrupted
                msg.push(workers.next());
                msg.send(backend);
            }

            workers.sendHeartbeats(backend);
            workers.purge();

        }

        // When we're done, clean up properly
        workers.close();
        context.destroy();
    }
}
