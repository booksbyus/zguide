package com.imatix.zguide.commons;

import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.stream.IntStream;

import static java.lang.String.format;
import static org.zeromq.ZMQ.PAIR;


public class ZHelper {

    private static Random RAND = new Random(System.currentTimeMillis());

    /**
     * Receives all message parts from socket, prints neatly
     */

    public static void dump(Socket socket) {
        System.out.println("===============================================");
        while (true) {
            byte[] msg = socket.recv(0);
            StringBuffer stringBuffer = new StringBuffer();
            IntStream.range(0, msg.length)
            .filter(i -> msg[i] > 32 || msg[i] < 127)
            .forEach(j -> stringBuffer.append(format("%02X", msg[j])));
            String data = stringBuffer.toString();
            System.out.println(format("[%03d] %s", msg.length, data));
            if (!socket.hasReceiveMore())
                break;
        }
    }

    static void setId(Socket socket) {
        socket.setIdentity(format("%04X-%04X", RAND.nextInt(), RAND.nextInt()).getBytes());
    }

    static List<Socket> buildZPipe(Context context) {
        Socket socket1 = context.socket(PAIR);
        socket1.setLinger(0);
        socket1.setHWM(1);

        Socket socket2 = context.socket(PAIR);
        socket2.setLinger(0);
        socket2.setHWM(1);

        String iface = String.format(ZConstants.URL.INPROC, new BigInteger(130, RAND).toString(32));
        socket1.bind(iface);
        socket2.connect(iface);
        return Arrays.asList(socket1, socket2);
    }
}
