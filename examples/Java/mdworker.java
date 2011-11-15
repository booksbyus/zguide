import org.zeromq.ZMsg;

/**
 * Majordomo Protocol worker example. Uses the mdwrk API to hide all MDP aspects
 * 
 * @author Arkadiusz Orzechowski <aorzecho@gmail.com>
 * 
 */
public class mdworker {

    /**
     * @param args
     */
    public static void main(String[] args) {
        boolean verbose = (args.length > 0 && "-v".equals(args[0]));
        mdwrkapi workerSession = new mdwrkapi("tcp://localhost:5555", "echo", verbose);

        ZMsg reply = null;
        while (!Thread.currentThread().isInterrupted()) {
            ZMsg request = workerSession.receive(reply);
            if (request == null)
                break; //Interrupted
            reply = request; //  Echo is complex... :-)
        }
        workerSession.destroy();
    }
}
