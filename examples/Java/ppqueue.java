import java.util.ArrayList;
import java.util.Iterator;

import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.PollItem;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZMsg;

//
// Paranoid Pirate queue
//

public class ppqueue {

    private final static int HEARTBEAT_LIVENESS = 3;       //  3-5 is reasonable
    private final static int HEARTBEAT_INTERVAL =  1000;    //  msecs

    //  Paranoid Pirate Protocol constants
    private final static String  PPP_READY     =  "\001";      //  Signals worker is ready
    private final static String  PPP_HEARTBEAT =  "\002";      //  Signals worker heartbeat
    
    //  Here we define the worker class; a structure and a set of functions that
    //  as constructor, destructor, and methods on worker objects:
    
    private static class Worker {
        ZFrame address;          //  Address of worker
        String identity;             //  Printable identity
        long expiry;             //  Expires at this time
        
        protected Worker(ZFrame address) {
            this.address = address;
            identity = new String(address.getData());
            expiry = System.currentTimeMillis() + HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS;
        }

        //  The ready method puts a worker to the end of the ready list:
        protected void ready(ArrayList<Worker> workers) {
            Iterator<Worker> it = workers.iterator();
            while (it.hasNext()) {
                Worker worker = it.next();
                if (identity.equals(worker.identity)) {
                    it.remove();
                    break;
                }
            }
            workers.add(this);
        }
        
        //  The next method returns the next available worker address:
        protected static ZFrame next(ArrayList<Worker> workers) {
            Worker worker = workers.remove(0);
            assert (worker != null);
            ZFrame frame = worker.address;
            return frame;
        }

        //  The purge method looks for and kills expired workers. We hold workers
        //  from oldest to most recent, so we stop at the first alive worker:
        protected static void purge(ArrayList<Worker> workers) {
            Iterator<Worker> it = workers.iterator();
            while (it.hasNext()) {
                Worker worker = it.next();
                if (System.currentTimeMillis() < worker.expiry) {
                    break;
                }
                it.remove();
            }
        }
    };

    //  The main task is an LRU queue with heartbeating on workers so we can
    //  detect crashed or blocked worker tasks:
    public static void main(String[] args) {
        ZContext ctx = new ZContext ();
        Socket frontend = ctx.createSocket(ZMQ.ROUTER);
        Socket backend = ctx.createSocket(ZMQ.ROUTER);
        frontend.bind( "tcp://*:5555");    //  For clients
        backend.bind( "tcp://*:5556");    //  For workers

        //  List of available workers
        ArrayList<Worker> workers = new ArrayList<Worker> ();

        //  Send out heartbeats at regular intervals
        long heartbeat_at = System.currentTimeMillis() + HEARTBEAT_INTERVAL;

        while (true) {
            PollItem items [] = {
                new PollItem( backend,  ZMQ.Poller.POLLIN ),
                new PollItem( frontend, ZMQ.Poller.POLLIN )
            };
            //  Poll frontend only if we have available workers
            int rc = ZMQ.poll (items, workers.size() > 0 ? 2:1,
                HEARTBEAT_INTERVAL );
            if (rc == -1)
                break;              //  Interrupted

            //  Handle worker activity on backend
            if (items [0].isReadable()) {
                //  Use worker address for LRU routing
                ZMsg msg = ZMsg.recvMsg (backend);
                if (msg == null)
                    break;          //  Interrupted

                //  Any sign of life from worker means it's ready
                ZFrame address = msg.unwrap();
                Worker worker = new Worker(address);
                worker.ready(workers);

                //  Validate control message, or return reply to client
                if (msg.size() == 1) {
                    ZFrame frame = msg.getFirst();
                    String data = new String(frame.getData());
                    if (!data.equals(PPP_READY)
                    &&  !data.equals( PPP_HEARTBEAT)) {
                        System.out.println ("E: invalid message from worker");
                        msg.dump(System.out);
                    }
                    msg.destroy();
                }
                else
                    msg.send(frontend);
            }
            if (items [1].isReadable()) {
                //  Now get next client request, route to next worker
                ZMsg msg = ZMsg.recvMsg (frontend);
                if (msg == null)
                    break;          //  Interrupted
                msg.push(Worker.next(workers));
                msg.send( backend);
            }

            //  We handle heartbeating after any socket activity. First we send
            //  heartbeats to any idle workers if it's time. Then we purge any
            //  dead workers:
            
            if (System.currentTimeMillis() >= heartbeat_at) {
                for (Worker worker: workers) {
                    
                    worker.address.send(backend,
                                 ZFrame.REUSE + ZFrame.MORE);
                    ZFrame frame = new ZFrame (PPP_HEARTBEAT);
                    frame.send(backend, 0);
                }
                heartbeat_at = System.currentTimeMillis() + HEARTBEAT_INTERVAL;
            }
            Worker.purge (workers);
        }

        //  When we're done, clean up properly
        while ( workers.size() > 0) {
            Worker worker = workers.remove(0);
        }
        workers.clear();
        ctx.destroy();
    }

}
