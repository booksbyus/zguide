//
//  Hello World server in Java
//  Binds REP socket to tcp://*:5555
//  Expects "Hello" from client, replies with "World"
//
//  Naveen Chawla <naveen.chwl@gmail.com>
//
import org.zeromq.ZMQ;

public class hwserver {
    public static void main(String[] args) {
        //  Prepare our context and socket
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket socket = context.socket(ZMQ.REP);
        socket.bind ("tcp://*:5555");

        while (true) {
            byte[] request;

            //  Wait for next request from client
            //  We will wait for a 0-terminated string (C string) from the client,
            //  so that this server also works with The Guide's C and C++ "Hello World" clients
            request = socket.recv (0);
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
            }

            //  Send reply back to client
            //  We will send a 0-terminated string (C string) back to the client,
            //  so that this server also works with The Guide's C and C++ "Hello World" clients
            String replyString = "World" + " ";
            byte[] reply = replyString.getBytes();
            reply[reply.length-1]=0; //Sets the last byte of the reply to 0
            socket.send(reply, 0);
        }
    }
}