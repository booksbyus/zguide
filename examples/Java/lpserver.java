package guide;

import java.util.Random;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZContext;

//
// Lazy Pirate server
// Binds REQ socket to tcp://*:5555
// Like hwserver except:
//  - echoes request as-is
//  - randomly runs slowly, or exits to simulate a crash.
//
public class lpserver
{

    public static void main(String[] argv) throws Exception
    {
        Random rand = new Random(System.nanoTime());

        try (ZContext context = new ZContext()) {
            Socket server = context.createSocket(SocketType.REP);
            server.bind("tcp://*:5555");

            int cycles = 0;
            while (true) {
                String request = server.recvStr();
                cycles++;

                //  Simulate various problems, after a few cycles
                if (cycles > 3 && rand.nextInt(3) == 0) {
                    System.out.println("I: simulating a crash");
                    break;
                }
                else if (cycles > 3 && rand.nextInt(3) == 0) {
                    System.out.println("I: simulating CPU overload");
                    Thread.sleep(2000);
                }
                System.out.printf("I: normal request (%s)\n", request);
                Thread.sleep(1000); //  Do some heavy work
                server.send(request);
            }
        }
    }
}
