/**
 *  Lazy Pirate server
 *  Binds REQ socket to tcp://*:5555
 *  Like hwserver except:
 *   - echoes request as-is
 *   - randomly runs slowly, or exits to simulate a crash.
 *
 *  @author Arkadiusz Orzechowski <aorzecho@gmail.com>
 */
import org.zeromq.ZMQ;
import java.util.Random;

public class lpserver {
    public static void main(String[] args) {
        // Prepare our context and socket
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket server = context.socket(ZMQ.REP);
        server.bind("tcp://*:5555");

        Random rand = new Random();
        int cycles = 0;
        while (true) {
            byte[] request = server.recv(0);
            cycles++;

            try {
                // Simulate various problems, after a few cycles
                if (cycles > 3 && rand.nextInt(4) == 0) {
                    System.out.println("I: simulating a crash");
                    break;
                } else if (cycles > 3 && rand.nextInt(4) == 0) {
                    System.out.println("I: simulating CPU overload");
                    Thread.sleep(2000);
                }

                System.out.printf("I: normal request (%s)\n", new String(
                        request).trim());
                // Do some 'work'
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            server.send(request, 0);
        }
        // cleanup
        server.close();
        context.term();
    }
}
