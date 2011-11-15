import org.zeromq.ZMsg;

/**
 * Majordomo Protocol client example, asynchronous. Uses the mdcli API to hide
 * all MDP aspects
 * 
 * @author Arkadiusz Orzechowski <aorzecho@gmail.com>
 * 
 */
public class mdclient2 {

    public static void main(String[] args) {
        boolean verbose = (args.length > 0 && "-v".equals(args[0]));
        mdcliapi2 clientSession = new mdcliapi2("tcp://localhost:5555", verbose);

        int count;
        for (count = 0; count < 100000; count++) {
            ZMsg request = new ZMsg();
            request.addString("Hello world");
            clientSession.send("echo", request);
        }
        for (count = 0; count < 100000; count++) {
            ZMsg reply = clientSession.recv();
            if (reply != null)
                reply.destroy();
            else
                break; // Interrupt or failure
        }

        System.out.printf("%d requests/replies processed\n", count);
        clientSession.destroy();
    }

}
