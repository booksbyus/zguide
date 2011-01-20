import org.zeromq.ZMQ;

/**
 * @author Faruk Akgul
 * @email faakgul@gmail.com
 */

public class durapub2 {

  public static void main(String[] args) throws InterruptedException {
    ZMQ.Context context = ZMQ.context(1);
    ZMQ.Socket sync = context.socket(ZMQ.PULL);
    ZMQ.Socket publisher = context.socket(ZMQ.PUB);

    // Subscriber tells us when it's ready
    sync.bind("tcp://*:5564");

    // We send updates via this socket
    publisher.bind("tcp://*:5565");

    // Prevent publisher overflow from slow subscribers
    publisher.setHWM(1);

    // Specify swap space in bytes, this covers all subscribers
    publisher.setSwap(25000000);

    // Wait for synchronization request
    sync.recv(0);

    // Now broadcast exactly 10 updates with pause
    for (int i = 0; i < 10; i++) {
      String msg = String.format("Update %d", i);
      publisher.send(msg.getBytes(), 0);
      Thread.sleep(1000);
    }
    publisher.send("END".getBytes(), 0);
    Thread.sleep(1000); // Give 0MQ/2.0.x time to flush output

  }

}

ribers
    publisher.setHWM(1);

    // Specify swap space in bytes, this covers all subscribers
    publisher.setSwap(25000000);

    // Wait for synchronization request
    sync.recv(0);

    // Now broadcast exactly 10 updates with pause
    for (int i = 0; i < 10; i++) {
      String msg = String.format("Update %d", i);
      publisher.send(msg.getBytes(), 0);
      Thread.sleep(1000);
    }
    publisher.send("END".getBytes(), 0);
    Thread.sleep(1000); // Give 0MQ/2.0.x time to flush outpu