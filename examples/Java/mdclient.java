package guide;

import org.zeromq.ZMsg;

/**
* Majordomo Protocol client example. Uses the mdcli API to hide all MDP aspects
*/
public class mdclient
{

    public static void main(String[] args)
    {
        boolean verbose = (args.length > 0 && "-v".equals(args[0]));
        mdcliapi clientSession = new mdcliapi("tcp://localhost:5555", verbose);

        int count;
        for (count = 0; count < 100000; count++) {
            ZMsg request = new ZMsg();
            request.addString("Hello world");
            ZMsg reply = clientSession.send("echo", request);
            if (reply != null)
                reply.destroy();
            else break; // Interrupt or failure
        }

        System.out.printf("%d requests/replies processed\n", count);
        clientSession.destroy();
    }

}
