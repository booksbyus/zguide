package com.imatix.zguide.ex1_10;

import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;

import static com.imatix.zguide.commons.ZConstants.URL.TCP_5558;
import static java.lang.System.currentTimeMillis;
import static org.zeromq.ZMQ.PULL;
import static org.zeromq.ZMQ.context;

/**
 * Task sink in Java
 * Binds PULL socket to tcp://localhost:5558
 * Collects results from workers via that socket
 *
 * @since 1.0
 */

public class TaskSink {

    public static void main(String[] args) {
        //  Prepare our context and socket
        Context context = context(1);
        Socket receiver = context.socket(PULL);
        receiver.bind(TCP_5558);
        //  Wait for start of batch
        String message = new String(receiver.recv(0));
        System.out.println("Start of the batch: " + message);
        //  Start our clock now
        long start = currentTimeMillis();
        //  Process 100 confirmations
        int taskNum;
        for (taskNum = 0; taskNum < 100; taskNum++) {
            new String(receiver.recv(0));
            if ((taskNum / 10) * 10 == taskNum)
                System.out.println(":");
            else
                System.out.println(".");
        }
        //  Calculate and report duration of batch
        long end = currentTimeMillis();
        System.out.printf("\nTotal elapsed time: %d msec.", (end - start));
        receiver.close();
        context.term();
    }
}
