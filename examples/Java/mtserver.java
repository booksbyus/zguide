/*
Multithreaded Hello World server in Java

Naveen Chawla <naveen.chwl@gmail.com>
*/
import org.zeromq.ZMQ;

public class MultiThreadedHelloWorldServer {
    public static void main(String[] args) {
        //  Prepare our context and sockets
        final ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket clients = context.socket(ZMQ.XREP);
        clients.bind ("tcp://*:5555");
        ZMQ.Socket workers = context.socket(ZMQ.XREQ);
        workers.bind ("inproc://workers");

        //  Launch pool of worker threads
        for (int thread_nbr = 0; thread_nbr != 5; thread_nbr++) {
            Thread worker_routine = new Thread(){
                                        public void run(){
                                            ZMQ.Socket socket = context.socket(ZMQ.REP);
                                            socket.connect ("inproc://workers");

                                            while (true) {
                                                //  Wait for next request from client (C string)
                                                byte[] request;
                                                request = socket.recv (0);
                                                System.out.println ("Received request: ["+new String(request,0,request.length-1)+"]");

                                                //  Do some 'work'
                                                try{
                                                    Thread.sleep (1000);
                                                }
                                                catch (InterruptedException e){
													e.printStackTrace();
												}

                                                //  Send reply back to client (C string)
                                                String replyString = "World" + " ";
                                                byte[] reply = replyString.getBytes ();
                                                reply[reply.length-1]=0; //Sets the last byte of the reply to 0
                                                socket.send (reply, 0);
                                            }
                                        }
                                    };
            worker_routine.start();
        }
        //  Connect work threads to client threads via a queue
        ZMQ.device(ZMQ.QUEUE, clients, workers);
    }
}
