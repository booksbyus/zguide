package guide;

import org.zeromq.ZMsg;

/**
 * MMI echo query example
 */
public class mmiecho
{

    public static void main(String[] args)
    {
        boolean verbose = (args.length > 0 && "-v".equals(args[0]));
        mdcliapi clientSession = new mdcliapi("tcp://localhost:5555", verbose);

        ZMsg request = new ZMsg();

        // This is the service we want to look up
        request.addString("echo");

        // This is the service we send our request to
        ZMsg reply = clientSession.send("mmi.service", request);

        if (reply != null) {
            String replyCode = reply.getFirst().toString();
            System.out.printf("Lookup echo service: %s\n", replyCode);
        }
        else {
            System.out.println("E: no response from broker, make sure it's running");
        }

        clientSession.destroy();
    }

}
