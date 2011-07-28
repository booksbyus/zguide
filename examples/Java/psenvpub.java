import org.zeromq.ZMQ;

/**
 * @author Faruk Akgul
 * @email faakgul@gmail.com
 */

public class psenvpub {

  public static void main(String[] args) throws InterruptedException {
    // Prepare our context and publisher
    ZMQ.Context context = ZMQ.context(1);
    ZMQ.Socket publisher = context.socket(ZMQ.PUB);

    publisher.bind("tcp://*:5563");
    while (true) {
      // Write two messages, each with an envelope and content
      publisher.send("A".getBytes(), ZMQ.SNDMORE);
      publisher.send("We don't want to see this".getBytes(), 0);
      publisher.send("B".getBytes(), ZMQ.SNDMORE);
      publisher.send("We would like to see this".getBytes(), 0);
      Thread.sleep(1000);
    }

  }
}