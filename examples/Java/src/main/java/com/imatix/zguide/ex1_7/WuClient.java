package com.imatix.zguide.ex1_7;

import com.imatix.zguide.commons.ZConstants;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;

import java.util.StringTokenizer;

import static java.lang.String.format;
import static org.zeromq.ZMQ.SUB;
import static org.zeromq.ZMQ.context;

/**
 * Weather update client in Java
 * Connects SUB socket to tcp://localhost:5556
 * Collects weather updates and finds avg temp in zipcode
 *
 * @since 1.0
 */

public class WuClient {

    public static void main(String[] args) {
        Context context = context(1);
        System.out.println("Collecting updates from weather update server...");
        //  Socket to talk to server
        Socket subscriber = context.socket(SUB);
        subscriber.connect(ZConstants.URL.TCP_5556);
        //  Subscribe to zipCode, default is NYC, 10001
        String filter = (args.length > 0) ? args[0] : "10001";
        subscriber.subscribe(filter.getBytes());
        // Process 100 updates
        long totalTemp = 0;
        int updateNum = 0;
        while (updateNum < 100) {
            //  Use trim to remove the tailing '0' character
            String receivedStr = subscriber.recvStr(0).trim();
            StringTokenizer tokenizer = new StringTokenizer(receivedStr, " ");
            int zipCode = Integer.valueOf(tokenizer.nextToken());
            int temperature = Integer.valueOf(tokenizer.nextToken());
            int relHumidity = Integer.valueOf(tokenizer.nextToken());
            System.out.println(format("Weather update: zipCode: %d , temperature: %d , humidity: %d",
                    zipCode, temperature, relHumidity));
            totalTemp += temperature;
            updateNum++;
        }
        System.out.printf("Average Temperature for zipCode %s was %d", filter, (int) (totalTemp / updateNum));
        subscriber.close();
        context.term();
    }
}
