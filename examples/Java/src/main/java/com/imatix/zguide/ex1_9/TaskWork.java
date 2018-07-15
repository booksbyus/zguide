package com.imatix.zguide.ex1_9;

import org.zeromq.ZMQ.*;

import static com.imatix.zguide.commons.ZConstants.URL.TCP_5557;
import static com.imatix.zguide.commons.ZConstants.URL.TCP_5558;
import static org.zeromq.ZMQ.*;

/**
 * Task worker in Java
 * Connects PULL socket to tcp://localhost:5557
 * Collects workloads from ventilator via that socket
 * Connects PUSH socket to tcp://localhost:5558
 * Sends results to sink via that socket
 *
 * @since 1.0
 */

public class TaskWork {

    public static void main(String[] args) throws InterruptedException {

        Context context = context(1);
        //  Socket to receive messages on
        Socket receiver = context.socket(PULL);
        receiver.connect(TCP_5557);
        //  Socket to send messages to
        Socket sender = context.socket(PUSH);
        sender.connect(TCP_5558);
        //  Process tasks forever
        while (!Thread.currentThread().isInterrupted()) {
            String duration = new String(receiver.recv(0)).trim();
            long msec = Long.parseLong(duration);
            //  Simple progress indicator for the viewer
            System.out.flush();
            System.out.println(duration + " . ");
            //  Do the work
            Thread.sleep(msec);
            //  Send results to sink
            sender.send("".getBytes(), 0);
        }
        sender.close();
        receiver.close();
        context.term();
    }

}
