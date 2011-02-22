import org.zeromq.ZMQ;

/**
 * @author Faruk Akgul
 * @email faakgul@gmail.com
 */

public class psenvsub {

  public static void main(String[] args) {

    // Prepare our context and subscriber
    ZMQ.Context context = ZMQ.context(1);
    ZMQ.Socket subscriber = context.socket(ZMQ.SUB);

    subscriber.connect("tcp://localhost:5563");
    subscriber.subscribe("B".getBytes());
    while (true) {
      // Read envelope with address
      String address = new String(subscriber.recv(0));
      // Read message contents
      String contents = new String(subscriber.recv(0));
      System.out.println(address + " : " + contents);
    }
  }
}