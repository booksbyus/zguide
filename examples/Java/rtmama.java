package guide;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

//
//Custom routing Router to Mama (ROUTER to REQ)
//
public class rtmama
{

    private static final int NBR_WORKERS = 10;

    public static class Worker implements Runnable
    {
        private final byte[] END = "END".getBytes(ZMQ.CHARSET);

        public void run()
        {
            try (ZContext context = new ZContext()) {
                ZMQ.Socket worker = context.createSocket(SocketType.REQ);
                // worker.setIdentity(); will set a random id automatically
                worker.connect("ipc://routing.ipc");

                int total = 0;
                while (true) {
                    worker.send("ready", 0);
                    byte[] workerload = worker.recv(0);
                    if (new String(workerload, ZMQ.CHARSET).equals("END")) {
                        System.out.println(String.format("Processs %d tasks.", total));
                        break;
                    }
                    total += 1;
                }
            }
        }
    }

    public static void main(String[] args)
    {
        try (ZContext context = new ZContext()) {
            ZMQ.Socket client = context.createSocket(SocketType.ROUTER);
            client.bind("ipc://routing.ipc");

            for (int i = 0; i != NBR_WORKERS; i++) {
                new Thread(new Worker()).start();
            }

            for (int i = 0; i != NBR_WORKERS; i++) {
                //  LRU worker is next waiting in queue
                byte[] address = client.recv(0);
                byte[] empty = client.recv(0);
                byte[] ready = client.recv(0);

                client.send(address, ZMQ.SNDMORE);
                client.send("", ZMQ.SNDMORE);
                client.send("This is the workload", 0);
            }

            for (int i = 0; i != NBR_WORKERS; i++) {
                //  LRU worker is next waiting in queue
                byte[] address = client.recv(0);
                byte[] empty = client.recv(0);
                byte[] ready = client.recv(0);

                client.send(address, ZMQ.SNDMORE);
                client.send("", ZMQ.SNDMORE);
                client.send("END", 0);
            }
        }
    }
}
