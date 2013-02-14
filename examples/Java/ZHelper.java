import org.zeromq.ZMQ.Socket;

import java.util.Random;

public class ZHelper
{
    private static Random rand = new Random(System.currentTimeMillis ());

    /**
     * Receives all message parts from socket, prints neatly
     */
    public static void dump (Socket sock)
    {
        System.out.println("----------------------------------------");
        while(true) {
            byte [] msg = sock.recv (0);
            boolean isText = true;
            String data = "";
            for (int i = 0; i< msg.length; i++) {
                if (msg[i] < 32 || msg[i] > 127)
                    isText = false;
                data += String.format ("%02X", msg[i]);
            }
            if (isText)
                data = new String (msg);

            System.out.println (String.format ("[%03d] %s", msg.length, data));
            if (!sock.hasReceiveMore ())
                break;
        }
    }

    public static void setId (Socket sock)
    {
        String identity = String.format ("%04X-%04X", rand.nextInt (), rand.nextInt ());

        sock.setIdentity (identity.getBytes ());
    }
}
