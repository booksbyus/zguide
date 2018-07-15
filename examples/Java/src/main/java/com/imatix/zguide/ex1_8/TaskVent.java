package com.imatix.zguide.ex1_8;

import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;

import java.io.IOException;
import java.util.Random;

import static com.imatix.zguide.commons.ZConstants.URL.TCP_5557;
import static com.imatix.zguide.commons.ZConstants.URL.TCP_5558;
import static java.lang.String.format;
import static org.zeromq.ZMQ.PUSH;
import static org.zeromq.ZMQ.context;

/**
 * Task ventilator in Java
 * Binds PUSH socket to tcp://localhost:5557
 * Sends batch of tasks to workers via that socket
 *
 * @since 1.0
 */

public class TaskVent {

    public static void main(String[] args) throws IOException, InterruptedException {
        Context context = context(1);

        //  Socket to send messages on
        Socket sender = context.socket(PUSH);
        sender.bind(TCP_5557);
        //  Socket to send messages on
        Socket sink = context.socket(PUSH);
        sink.connect(TCP_5558); // Why connect ?
        System.out.println("Press Enter as soon as the workers are all set: ");
        System.in.read();
        System.out.println("Sending Tasks to the workers ...");
        //  The first message is "0" and signals start of batch
        sink.send("0", 0);
        //  Initialize random number generator
        Random random = new Random(System.currentTimeMillis());
        //  Send 100 tasks
        int totalDuration = 0;
        for (int taskNum = 0; taskNum < 100; taskNum++) {
            int workload;
            //  Random workload from 1 to 100 msecs.
            workload = random.nextInt(100) + 1;
            totalDuration += workload;
            System.out.println(workload + ".");
            String taskDuration = format("%d", workload);
            sender.send(taskDuration, 0);
        }

        System.out.printf("\nTotal expected cost: %d msec.", totalDuration);
        //  Give 0MQ time to deliver
        Thread.sleep(1000);
        sink.close();
        sender.close();
        context.term();
    }
}
