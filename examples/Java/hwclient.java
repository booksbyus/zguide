//
//  Hello World client in Java
//  Connects REQ socket to tcp://localhost:5555
//  Sends "Hello" to server, expects "World" back
//

import org.zeromq.ZMQ;

public class hwclient{

    public static void main (String[] args){
        //  Prepare our context and socket
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket socket = context.socket(ZMQ.REQ);
        socket.connect ("tcp://localhost:5555");

        System.out.println("Connecting to hello world server");

        //  Do 10 requests, waiting each time for a response
        for(int requestNbr = 0; requestNbr != 10; requestNbr++) {
            //  Create a "Hello" message.
            //  Ensure that the last byte of our "Hello" message is 0 because
            //  our "Hello World" server is expecting a 0-terminated string:
            String requestString = "Hello" ;
            byte[] request = requestString.getBytes();
            // Send the message
            System.out.println("Sending request " + requestNbr );
            socket.send(request, 0);

            //  Get the reply.
            byte[] reply = socket.recv(0);
            //  When displaying reply as a String, omit the last byte because
            //  our "Hello World" server has sent us a 0-terminated string:
            System.out.println("Received reply " + requestNbr + ": [" + new String(reply) + "]");
        }
        
        socket.close();
        context.term();
    }
}
