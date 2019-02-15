package guide;

import org.zeromq.*;
import org.zeromq.ZMQ.Socket;

/**
* Round-trip demonstrator. Broker, Worker and Client are mocked as separate
* threads.
*/
public class tripping
{
    static class Broker implements Runnable
    {
        @Override
        public void run()
        {
            try (ZContext ctx = new ZContext()) {
                Socket frontend = ctx.createSocket(SocketType.ROUTER);
                Socket backend = ctx.createSocket(SocketType.ROUTER);
                frontend.setHWM(0);
                backend.setHWM(0);
                frontend.bind("tcp://*:5555");
                backend.bind("tcp://*:5556");

                while (!Thread.currentThread().isInterrupted()) {
                    ZMQ.Poller items = ctx.createPoller(2);
                    items.register(frontend, ZMQ.Poller.POLLIN);
                    items.register(backend, ZMQ.Poller.POLLIN);

                    if (items.poll() == -1)
                        break; // Interrupted

                    if (items.pollin(0)) {
                        ZMsg msg = ZMsg.recvMsg(frontend);
                        if (msg == null)
                            break; // Interrupted
                        ZFrame address = msg.pop();
                        address.destroy();
                        msg.addFirst(new ZFrame("W"));
                        msg.send(backend);
                    }

                    if (items.pollin(1)) {
                        ZMsg msg = ZMsg.recvMsg(backend);
                        if (msg == null)
                            break; // Interrupted
                        ZFrame address = msg.pop();
                        address.destroy();
                        msg.addFirst(new ZFrame("C"));
                        msg.send(frontend);
                    }

                    items.close();
                }
            }
        }
    }

    static class Worker implements Runnable
    {
        @Override
        public void run()
        {
            try (ZContext ctx = new ZContext()) {
                Socket worker = ctx.createSocket(SocketType.DEALER);
                worker.setHWM(0);
                worker.setIdentity("W".getBytes(ZMQ.CHARSET));
                worker.connect("tcp://localhost:5556");
                while (!Thread.currentThread().isInterrupted()) {
                    ZMsg msg = ZMsg.recvMsg(worker);
                    msg.send(worker);
                }
            }
        }
    }

    static class Client implements Runnable
    {
        private static int SAMPLE_SIZE = 10000;

        @Override
        public void run()
        {
            try (ZContext ctx = new ZContext()) {
                Socket client = ctx.createSocket(SocketType.DEALER);
                client.setHWM(0);
                client.setIdentity("C".getBytes(ZMQ.CHARSET));
                client.connect("tcp://localhost:5555");
                System.out.println("Setting up test");
                try {
                    Thread.sleep(100);
                }
                catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }

                int requests;
                long start;

                System.out.println("Synchronous round-trip test");
                start = System.currentTimeMillis();

                for (requests = 0; requests < SAMPLE_SIZE; requests++) {
                    ZMsg req = new ZMsg();
                    req.addString("hello");
                    req.send(client);
                    ZMsg.recvMsg(client).destroy();
                }

                long now = System.currentTimeMillis();
                System.out.printf(
                    " %d calls/second\n", (1000 * SAMPLE_SIZE) / (now - start)
                );

                System.out.println("Asynchronous round-trip test");
                start = System.currentTimeMillis();

                for (requests = 0; requests < SAMPLE_SIZE; requests++) {
                    ZMsg req = new ZMsg();
                    req.addString("hello");
                    req.send(client);
                }

                for (requests = 0;
                     requests < SAMPLE_SIZE && !Thread.currentThread()
                                                      .isInterrupted();
                     requests++) {
                    ZMsg.recvMsg(client).destroy();
                }

                long now2 = System.currentTimeMillis();
                System.out.printf(
                    " %d calls/second\n", (1000 * SAMPLE_SIZE) / (now2 - start)
                );
            }
        }
    }

    public static void main(String[] args)
    {
        if (args.length == 1)
            Client.SAMPLE_SIZE = Integer.parseInt(args[0]);

        Thread brokerThread = new Thread(new Broker());
        Thread workerThread = new Thread(new Worker());
        Thread clientThread = new Thread(new Client());

        brokerThread.setDaemon(true);
        workerThread.setDaemon(true);

        brokerThread.start();
        workerThread.start();
        clientThread.start();

        try {
            clientThread.join();
            workerThread.interrupt();
            brokerThread.interrupt();
            Thread.sleep(200);// give them some time
        }
        catch (InterruptedException e) {
        }
    }
}
