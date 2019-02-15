package guide;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

/**
 * Task sink - design 2
 * Adds pub-sub flow to send kill signal to workers
 */
public class tasksink2
{
    public static void main(String[] args) throws Exception
    {
        //  Prepare our context and socket
        try (ZContext context = new ZContext()) {
            ZMQ.Socket receiver = context.createSocket(SocketType.PULL);
            receiver.bind("tcp://*:5558");

            // Socket for worker control
            ZMQ.Socket controller = context.createSocket(SocketType.PUB);
            controller.bind("tcp://*:5559");

            //  Wait for start of batch
            receiver.recv(0);

            //  Start our clock now
            long tstart = System.currentTimeMillis();

            //  Process 100 confirmations
            int task_nbr;
            for (task_nbr = 0; task_nbr < 100; task_nbr++) {
                receiver.recv(0);
                if ((task_nbr / 10) * 10 == task_nbr) {
                    System.out.print(":");
                }
                else {
                    System.out.print(".");
                }
                System.out.flush();
            }
            //  Calculate and report duration of batch
            long tend = System.currentTimeMillis();

            System.out.println(
                "Total elapsed time: " + (tend - tstart) + " msec"
            );

            //  Send the kill signal to the workers
            controller.send("KILL", 0);

            //  Give it some time to deliver
            Thread.sleep(1);
        }
    }
}
