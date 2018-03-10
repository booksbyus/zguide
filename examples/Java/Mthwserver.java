//
//  Multithreaded Hello World server in Java
//
//  Arnaud Cogoluègnes <acogoluegnes@gmail.com>
//
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQQueue;

public class Mthwserver {
	
	private static final class WorkerThread extends Thread {
		
		private final ZMQ.Context context;		
		
		public WorkerThread(Context context) {
			super();
			this.context = context;
		}

		@Override
		public void run() {
			ZMQ.Socket receiver = context.socket(ZMQ.REP);
			receiver.connect("inproc://workers");
			while(!Thread.currentThread().isInterrupted()) {
				byte[] request = receiver.recv (0);
	            //  In order to display the 0-terminated string as a String,
	            //  we omit the last byte from request
	            System.out.println ("Received request: [" +
	            new String(request,0,request.length-1)  //  Creates a String from request, minus the last byte
	            + "]");

	            //  Do some 'work'
	            try {
	                Thread.sleep (1000);
	            }
	            catch(InterruptedException e){
	                e.printStackTrace();
	                Thread.currentThread().interrupt();
	            }

	            //  Send reply back to client
	            //  We will send a 0-terminated string (C string) back to the client,
	            //  so that this server also works with The Guide's C and C++ "Hello World" clients
	            String replyString = "World" + " ";
	            byte[] reply = replyString.getBytes();
	            reply[reply.length-1]=0; //Sets the last byte of the reply to 0
	            receiver.send(reply, 0);
			}
		}
		
	}
	
    public static void main(String[] args) {
        //  Prepare our context and socket
        ZMQ.Context context = ZMQ.context(1);
        // Socket to talk to clients
        ZMQ.Socket clients = context.socket(ZMQ.ROUTER);
        clients.bind("tcp://*:5555");
        
        // Socket to talk to workers
        ZMQ.Socket workers = context.socket(ZMQ.DEALER);
        workers.bind("inproc://workers");
        
        // Launch worker threads
        for(int threadNb = 0;threadNb < 5; threadNb++) {
        	new WorkerThread(context).start();
        }
        // Connect work threads to client threads via a queue
        ZMQQueue queue = new ZMQQueue(context, clients, workers);
        new Thread(queue).start();
    }
}
