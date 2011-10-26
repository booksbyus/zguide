/**
 *  Simple Pirate worker
 *  Connects REQ socket to tcp://*:5556
 *  Implements worker part of LRU queueing
 *
 *  @author Arkadiusz Orzechowski <aorzecho@gmail.com>
 */
import org.zeromq.ZMQ;
import java.util.Random;

public class spworker {

    private static final byte[] LRU_READY = { 1 }; // Signals worker is ready

    public static void main(String[] args) {
        // Prepare our context and socket
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket worker = context.socket(ZMQ.REQ);

        // Set random identity to make tracing easier
        Random rand = new Random();
        String id = String.format("%04x-%04x", rand.nextInt(0x10001),
                rand.nextInt(0x10001));
        worker.setIdentity(id.getBytes());
        worker.connect("tcp://localhost:5556");

        // Tell broker we're ready for work
        System.out.printf("I: (%s) worker ready\n", id);
        worker.send(LRU_READY, 0);

        int cycles = 0;
        while (!Thread.currentThread().isInterrupted()) {
            // we are expecting [clientAddr][][request]
            // need some higher-level api for multipart messages in Java
            byte[] clientAddr = worker.recv(0);
            byte[] empty = worker.recv(0);
            byte[] request = worker.recv(0);
            cycles++;
            try {
                // Simulate various problems, after a few cycles
                if (cycles > 3 && rand.nextInt(6) == 0) {
                    System.out.printf("I: (%s) simulating a crash\n", id);
                    break;
                } else if (cycles > 3 && rand.nextInt(6) == 0) {
                    System.out.printf("I: (%s) simulating CPU overload\n", id);
                    Thread.sleep(3000);
                }

                System.out.printf("I: (%s) normal reply\n", id);
                // Do some 'work'
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            worker.send(clientAddr, ZMQ.SNDMORE);
            worker.send(empty, ZMQ.SNDMORE);
            worker.send(request, 0);
        }
        // cleanup
        worker.close();
        context.term();
    }
}
