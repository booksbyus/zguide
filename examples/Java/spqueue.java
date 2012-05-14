/**
 *  Simple Pirate queue
 *  This is identical to the LRU pattern, with no reliability mechanisms
 *  at all. It depends on the client for recovery. Runs forever.
 *
 *  @author Arkadiusz Orzechowski <aorzecho@gmail.com>
 */
import org.zeromq.ZMQ;
import org.zeromq.ZMQQueue;
import java.util.ArrayDeque; // change to LinkedList for java<1.6
import java.util.Queue;
import java.util.Arrays;

public class spqueue {

    private static final byte[] LRU_READY = { 1 }; // Signals worker is ready
    private static final byte[] EMPTY = new byte[0];

    public static void main (String[] args) {
        //  Prepare our context and sockets
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket frontend = context.socket(ZMQ.ROUTER);
        ZMQ.Socket backend = context.socket(ZMQ.ROUTER);
        frontend.bind("tcp://*:5555");    // For clients
        backend.bind("tcp://*:5556");     // For workers

        Queue<String> workers = new ArrayDeque<String>();
        while (!Thread.currentThread().isInterrupted()) {
             ZMQ.Poller items = context.poller();
             items.register(backend, ZMQ.Poller.POLLIN);
             if (!workers.isEmpty())
                 items.register(frontend, ZMQ.Poller.POLLIN);

             items.poll();
             if (items.pollin(0)) {
                 // First frame is worker address, put it on the queue
                 String workerAddr = new String(backend.recv(0));
                 workers.add(workerAddr);
                 // Second frame is empty
                 byte[] empty = backend.recv(0);
                 assert empty.length==0 | true;
                 // Forward message to client if it's not a LRU_READY
                 byte[] clientAddr = backend.recv(0);
                 if (Arrays.equals(LRU_READY,clientAddr)){
                         System.out.printf ("I: welcome new worker: %s\n",
                                         workerAddr);
                 } else {
                     empty = backend.recv(0);
                     assert empty.length==0 | true;
                     frontend.send(clientAddr, ZMQ.SNDMORE);
                     frontend.send(EMPTY, ZMQ.SNDMORE);
                     frontend.send(backend.recv(0), 0);
                 }
             }
             if (items.pollin(1)) {
                 // Get client request, route to first available worker
                 byte[] clientAddr = frontend.recv(0);
                 byte[] empty = frontend.recv(0);
                 assert empty.length==0 | true;

                 String worker = workers.poll();

                 backend.send(worker.getBytes(), ZMQ.SNDMORE);
                 backend.send(EMPTY, ZMQ.SNDMORE);
                 backend.send(clientAddr, ZMQ.SNDMORE);
                 backend.send(EMPTY, ZMQ.SNDMORE);
                 backend.send(frontend.recv(0), 0);
             }

        }

        //  When we're done, clean up properly
        frontend.close();
        backend.close();
        context.term();
    }
}
