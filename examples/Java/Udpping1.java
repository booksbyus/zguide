import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.PollItem;
import org.zeromq.ZMQ.Poller;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.StandardSocketOptions;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;

/**
 * UDP ping command
 * Model 1, does UDP work inline
 */
public class Udpping1 {
  private static final int PING_PORT_NUMBER = 9999;
  private static final int PING_MSG_SIZE = 1;
  private static final int PING_INTERVAL = 1000; //  Once per second

  public static void main(String[] args) throws IOException {
    ZContext ctx = new ZContext();

    //  Create UDP socket
    DatagramChannel udpChannel = DatagramChannel.open();

    //  Ask operating system to let us do broadcasts from socket
    udpChannel.setOption(StandardSocketOptions.SO_BROADCAST, true);
    udpChannel.configureBlocking(false);
    //  Bind UDP socket to local port so we can receive pings
    udpChannel.bind(new InetSocketAddress(PING_PORT_NUMBER));
    ByteBuffer buffer = ByteBuffer.allocate(PING_MSG_SIZE);

    //  We use zmq_poll to wait for activity on the UDP socket, because
    //  this function works on non-0MQ file handles. We send a beacon
    //  once a second, and we collect and report beacons that come in
    //  from other nodes:
    PollItem[] pollItems = new PollItem[]{new PollItem(udpChannel, Poller.POLLIN)};
    //  Send first ping right away
    long pingAt = System.currentTimeMillis();

    while (!Thread.currentThread().isInterrupted()) {
      long timeout = pingAt - System.currentTimeMillis();
      if (timeout < 0)
        timeout = 0;
      if (ZMQ.poll(pollItems, 1, timeout) == -1)
        break;              //  Interrupted

      //  Someone answered our ping
      if (pollItems[0].isReadable()) {
        InetSocketAddress sourceAddr = (InetSocketAddress) udpChannel.receive(buffer);
        System.out.printf("Found peer %s:%d\n", sourceAddr.getAddress(), sourceAddr.getPort());
      }
      if (System.currentTimeMillis() >= pingAt) {
        //  Broadcast our beacon
        System.out.println("Pinging peers…");
        buffer.put(0, (byte) '!');
        udpChannel.send(buffer, new InetSocketAddress("255.255.255.255", PING_PORT_NUMBER));
        pingAt = System.currentTimeMillis() + PING_INTERVAL;
      }
    }
    udpChannel.close();
    ctx.destroy();
  }
}
