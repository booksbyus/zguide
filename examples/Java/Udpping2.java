import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.io.IOException;
import java.net.SocketAddress;
import java.nio.ByteBuffer;

/**
 * UDP ping command
 * Model 2, uses separate UDP library
 */
public class Udpping2 {
  private static final int PING_PORT_NUMBER = 9999;
  private static final int PING_MSG_SIZE = 1;
  private static final int PING_INTERVAL = 1000; //  Once per second

  public static void main(String[] args) throws IOException {
    ZContext ctx = new ZContext();
    Udplib udp = new Udplib(PING_PORT_NUMBER);

    ByteBuffer buffer = ByteBuffer.allocate(PING_MSG_SIZE);
    ZMQ.PollItem[] pollItems = new ZMQ.PollItem[]{
            new ZMQ.PollItem(udp.handle(), ZMQ.Poller.POLLIN)};
    //  Send first ping right away
    long pingAt = System.currentTimeMillis();

    while (!Thread.currentThread().isInterrupted()) {
      long timeout = pingAt - System.currentTimeMillis();
      if (timeout < 0)
        timeout = 0;
      if (ZMQ.poll(pollItems, 1, timeout) == -1)
        break;              //  Interrupted

      //  Someone answered our ping
      if (pollItems[0].isReadable()){
        SocketAddress sourceAddr = udp.recv(buffer);
        System.out.printf("new peer found: %s\n",sourceAddr);
      }

      if (System.currentTimeMillis() >= pingAt) {
        System.out.println("Pinging peers…");
        buffer.put(0, (byte) '!');
        udp.send(buffer);
        pingAt = System.currentTimeMillis() + PING_INTERVAL;
      }
    }
    udp.destroy();
    ctx.destroy();
  }
}
