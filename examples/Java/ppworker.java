import java.util.Random;

import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.PollItem;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZMsg;

//
// Paranoid Pirate worker
//
public class ppworker {

    private final static int HEARTBEAT_LIVENESS = 3 ;      //  3-5 is reasonable
    private final static int HEARTBEAT_INTERVAL = 1000;    //  msecs
    private final static int INTERVAL_INIT      = 1000;    //  Initial reconnect
    private final static int INTERVAL_MAX      = 32000;    //  After exponential backoff

    //  Paranoid Pirate Protocol constants
    private final static String PPP_READY     =   "\001" ;     //  Signals worker is ready
    private final static String PPP_HEARTBEAT =   "\002" ;     //  Signals worker heartbeat
    
    //  Helper function that returns a new configured socket
    //  connected to the Paranoid Pirate queue
    
    private static Socket worker_socket(ZContext ctx) {
        Socket worker = ctx.createSocket(ZMQ.DEALER);
        worker.connect( "tcp://localhost:5556");

        //  Tell queue we're ready for work
        System.out.println ("I: worker ready\n");
        ZFrame frame = new ZFrame (PPP_READY);
        frame.send( worker, 0);

        return worker;
    }
    
    //  We have a single task, which implements the worker side of the
    //  Paranoid Pirate Protocol (PPP). The interesting parts here are
    //  the heartbeating, which lets the worker detect if the queue has
    //  died, and vice-versa:
    
    public static void main(String[] args) {
        ZContext ctx = new ZContext ();
        Socket worker = worker_socket (ctx);

        //  If liveness hits zero, queue is considered disconnected
        int liveness = HEARTBEAT_LIVENESS;
        int interval = INTERVAL_INIT;

        //  Send out heartbeats at regular intervals
        long heartbeat_at = System.currentTimeMillis() + HEARTBEAT_INTERVAL;

        Random rand = new Random(System.nanoTime());
        int cycles = 0;
        while (true) {
            PollItem items [] = { new PollItem( worker,  ZMQ.Poller.POLLIN ) };
            int rc = ZMQ.poll (items, HEARTBEAT_INTERVAL );
            if (rc == -1)
                break;              //  Interrupted

            if (items [0].isReadable()) {
                //  Get message
                //  - 3-part envelope + content -> request
                //  - 1-part HEARTBEAT -> heartbeat
                ZMsg msg = ZMsg.recvMsg(worker);
                if (msg == null)
                    break;          //  Interrupted

                //  To test the robustness of the queue implementation we //
                //  simulate various typical problems, such as the worker
                //  crashing, or running very slowly. We do this after a few
                //  cycles so that the architecture can get up and running
                //  first:
                if (msg.size() == 3) {
                    cycles++;
                    if (cycles > 3 && rand.nextInt(5) == 0) {
                        System.out.println ("I: simulating a crash\n");
                        msg.destroy();
                        msg = null;
                        break;
                    }
                    else
                    if (cycles > 3 && rand.nextInt (5) == 0) {
                        System.out.println ("I: simulating CPU overload\n");
                        try {
                            Thread.sleep (3000);
                        } catch (InterruptedException e) {
                            break;
                        }
                    }
                    System.out.println ("I: normal reply\n");
                    msg.send( worker);
                    liveness = HEARTBEAT_LIVENESS;
                    try {
                        Thread.sleep (1000);
                    } catch (InterruptedException e) {
                        break;
                    }              //  Do some heavy work
                }
                else
                //  When we get a heartbeat message from the queue, it means the
                //  queue was (recently) alive, so reset our liveness indicator:
                if (msg.size() == 1) {
                    ZFrame frame = msg.getFirst();
                    if (PPP_HEARTBEAT.equals(new String(frame.getData())))
                        liveness = HEARTBEAT_LIVENESS;
                    else {
                        System.out.println ("E: invalid message\n");
                        msg.dump(System.out);
                    }
                    msg.destroy();
                }
                else {
                    System.out.println ("E: invalid message\n");
                    msg.dump(System.out);
                }
                interval = INTERVAL_INIT;
            }
            else
            //  If the queue hasn't sent us heartbeats in a while, destroy the
            //  socket and reconnect. This is the simplest most brutal way of
            //  discarding any messages we might have sent in the meantime://
            if (--liveness == 0) {
                System.out.println ("W: heartbeat failure, can't reach queue\n");
                System.out.println (String.format("W: reconnecting in %zd msec\n", interval));
                try {
                    Thread.sleep(interval);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }

                if (interval < INTERVAL_MAX)
                    interval *= 2;
                ctx.destroySocket(worker);
                worker = worker_socket (ctx);
                liveness = HEARTBEAT_LIVENESS;
            }

            //  Send heartbeat to queue if it's time
            if (System.currentTimeMillis() > heartbeat_at) {
                heartbeat_at = System.currentTimeMillis() + HEARTBEAT_INTERVAL;
                System.out.println ("I: worker heartbeat\n");
                ZFrame frame = new ZFrame (PPP_HEARTBEAT);
                frame.send(worker, 0);
            }
        }
        ctx.destroy();
    }

}
