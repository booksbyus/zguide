//
//  Multithreaded relay in Java
//
//  Naveen Chawla <naveen.chwl@gmail.com>
//
import org.zeromq.ZMQ;

public class mtrelay{
    public static void main(String[] args) {
        final ZMQ.Context context = ZMQ.context(1);

        //  Bind to inproc: endpoint, then start upstream thread
        ZMQ.Socket receiver = context.socket(ZMQ.PAIR);
        receiver.bind("inproc://step3");
        
        //  Step 2 relays the signal to step 3
        Thread step2 = 	new Thread(){
                            public void run(){
                                //  Bind to inproc: endpoint, then start upstream thread
                                ZMQ.Socket receiver = context.socket(ZMQ.PAIR);
                                receiver.bind("inproc://step2");
                                Thread step1 = new Thread(){
                                                   public void run(){
                                                       //  Signal downstream to step 2
                                                       ZMQ.Socket sender = context.socket(ZMQ.PAIR);
                                                       sender.connect("inproc://step2");
                                                       sender.send("".getBytes(),0);
                                                   }
                                               };
				                
                                step1.start();
				                
                                //  Wait for signal
                                byte[] message;
                                message=receiver.recv(0);

                                //  Signal downstream to step 3
                                ZMQ.Socket sender = context.socket(ZMQ.PAIR);
                                sender.connect("inproc://step3");
                                sender.send(message,0);
                            }
                        };
        step2.start();
        
        //  Wait for signal
        byte[] message;
        message = receiver.recv(0);

        System.out.println ("Test successful!");
    }
}