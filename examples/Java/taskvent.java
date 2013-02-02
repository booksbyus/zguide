import java.util.Random;
import org.zeromq.ZMQ;

//
//  Task ventilator in Java
//  Binds PUSH socket to tcp://localhost:5557
//  Sends batch of tasks to workers via that socket
//
public class taskvent {

    public static void main (String[] args) throws Exception {
        ZMQ.Context context = ZMQ.context(1);

        //  Socket to send messages on
        ZMQ.Socket sender = context.socket(ZMQ.PUSH);
        sender.bind("tcp://*:5557");

        //  Socket to send messages on
        ZMQ.Socket sink = context.socket(ZMQ.PUSH);
        sink.connect("tcp://localhost:5558");

        System.out.println("Press Enter when the workers are ready: ");
        System.in.read();
        System.out.println("Sending tasks to workers\n");

        //  The first message is "0" and signals start of batch
        sink.send("0", 0);

        //  Initialize random number generator
        Random srandom = new Random(System.currentTimeMillis());

        //  Send 100 tasks
        int task_nbr;
        int total_msec = 0;     //  Total expected cost in msecs
        for (task_nbr = 0; task_nbr < 100; task_nbr++) {
            int workload;
            //  Random workload from 1 to 100msecs
            workload = srandom.nextInt(100) + 1;
            total_msec += workload;
            System.out.print(workload + ".");
            String string = String.format("%d", workload);
            sender.send(string, 0);
        }
        System.out.println("Total expected cost: " + total_msec + " msec");
        Thread.sleep(1000);              //  Give 0MQ time to deliver

        sink.close();
        sender.close();
        context.term();
    }
}
