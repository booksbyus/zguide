import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.StandardSocketOptions;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;

public class Udplib {
  private int port;
  private DatagramChannel handle;

  /**
   * Constructor
   *
   * @param port udp port to use
   */
  public Udplib(int port) throws IOException {
    this.port = port;
    //  Create UDP socket
    handle = DatagramChannel.open();

    //  Ask operating system to let us do broadcasts from socket
    handle.setOption(StandardSocketOptions.SO_BROADCAST, true);
    handle.configureBlocking(false);

    //  Allow multiple processes to bind to socket; incoming
    //  messages will come to each process
    handle.setOption(StandardSocketOptions.SO_REUSEADDR, true);

    handle.bind(new InetSocketAddress(port));
  }

  /**
   * Returns UDP socket handle
   */
  public DatagramChannel handle() {
    return handle;
  }

  /**
   * Send message using UDP broadcast
   */
  public void send(ByteBuffer buffer) throws IOException {
    handle.send(buffer, new InetSocketAddress("255.255.255.255", port));
  }

  /**
   * Receive message from UDP broadcast
   *
   * @return source of the datagram or null if there isn't any datagram to receive
   */
  public SocketAddress recv(ByteBuffer buffer) throws IOException {
    return handle.receive(buffer);
  }

  /**
   * Destructor
   */
  public void destroy() throws IOException {
    handle.close();
  }
}
