package guide;

import java.util.Random;

import org.zeromq.*;
import org.zeromq.ZMQ.Socket;

//
// Simple Pirate worker
// Connects REQ socket to tcp://*:5556
// Implements worker part of load-balancing queueing
//
public class spworker
{

    private final static String WORKER_READY = "\001"; //  Signals worker is ready

    public static void main(String[] args) throws Exception
    {
        try (ZContext ctx = new ZContext()) {
            Socket worker = ctx.createSocket(SocketType.REQ);

            //  Set random identity to make tracing easier
            Random rand = new Random(System.nanoTime());
            String identity = String.format(
                "%04X-%04X", rand.nextInt(0x10000), rand.nextInt(0x10000)
            );
            worker.setIdentity(identity.getBytes(ZMQ.CHARSET));
            worker.connect("tcp://localhost:5556");

            //  Tell broker we're ready for work
            System.out.printf("I: (%s) worker ready\n", identity);
            ZFrame frame = new ZFrame(WORKER_READY);
            frame.send(worker, 0);

            int cycles = 0;
            while (true) {
                ZMsg msg = ZMsg.recvMsg(worker);
                if (msg == null)
                    break; //  Interrupted

                //  Simulate various problems, after a few cycles
                cycles++;
                if (cycles > 3 && rand.nextInt(5) == 0) {
                    System.out.printf("I: (%s) simulating a crash\n", identity);
                    msg.destroy();
                    break;
                }
                else if (cycles > 3 && rand.nextInt(5) == 0) {
                    System.out.printf(
                        "I: (%s) simulating CPU overload\n", identity
                    );
                    Thread.sleep(3000);
                }
                System.out.printf("I: (%s) normal reply\n", identity);
                Thread.sleep(1000); //  Do some heavy work
                msg.send(worker);
            }
        }
    }
}
