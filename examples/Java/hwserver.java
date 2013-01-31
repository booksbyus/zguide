//
//  Hello World server in Java
//  Binds REP socket to tcp://*:5555
//  Expects "Hello" from client, replies with "World"
//

import org.zeromq.ZMQ;

public class hwserver{

    public static void main (String[] args) throws Exception{
        //  Prepare our context and socket
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket socket = context.socket(ZMQ.REP);

        System.out.println("Binding hello world server");
        socket.bind ("tcp://*:5555");

        while (!Thread.currentThread ().isInterrupted ()) {

            // Wait for next request from client
            byte[] reply = socket.recv(0);
            System.out.println("Received " + reply.length );
            System.out.println("Received " + ": [" + new String(reply) + "]");

            Thread.sleep(1000);
            //  Create a "Hello" message.
            //  Ensure that the last byte of our "Hello" message is 0 because
            //  our "Hello World" server is expecting a 0-terminated string:
            String requestString = "Hello" ;
            byte[] request = requestString.getBytes();
            //request[request.length-1]=0; //Sets the last byte to 0
            // Send the message
            System.out.println("Sending response " + requestString );
            socket.send(request, 0);

            //  Get the reply.
        }
        
        socket.close();
        context.term();
    }
}
