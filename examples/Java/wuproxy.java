import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;

/**
* Weather proxy device.
*/
public class wuproxy{

    public static void main (String[] args) {
        //  Prepare our context and sockets
        Context context = ZMQ.context(1);

        //  This is where the weather server sits
        Socket frontend =  context.socket(ZMQ.SUB);
        frontend.connect("tcp://192.168.55.210:5556");

        //  This is our public endpoint for subscribers
        Socket backend  = context.socket(ZMQ.PUB);
        backend.bind("tcp://10.1.1.0:8100");

        //  Subscribe on everything
        frontend.subscribe("".getBytes());

        //  Run the proxy until the user interrupts us
        ZMQ.proxy (frontend, backend, null);

        frontend.close();
        backend.close();
        context.term();
    }
}
