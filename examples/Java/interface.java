import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMsg;
import org.zeromq.ZThread;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Interface class for Chapter on Distributed Computing
 * This implements an "interface" to our network of nodes
 */
public class Interface {
  ZContext ctx; //  Our context wrapper
  ZMQ.Socket pipe;


  public Interface() {
    ctx = new ZContext();
    pipe = ZThread.fork(ctx, new InterfaceAgent());
  }

  /**
   * Here we wait for a message from the interface.
   *
   * @return Zmsg object or null if interrupted
   */
  public ZMsg recv() {
    return ZMsg.recvMsg(pipe);
  }

  public void destroy() {
    this.ctx.destroy();
  }

  /**
   * Asynchronous part, works in the background
   */
  private static class InterfaceAgent implements ZThread.IAttachedRunnable {
    private static final int PING_PORT_NUMBER = 9999;
    private static final int PING_INTERVAL = 1000; //  Once per second
    private static final byte[] emptyUUID = new byte[16];
    private Udplib udp;                 //  UDP object
    private UUID uuid;                //  Our UUID as binary blob
    private Map<UUID, Peer> peers;
    private ZMQ.Socket pipe;                 //  Pipe back to application

    private static ByteBuffer serializeUUID(UUID obj) {
      ByteBuffer result = ByteBuffer.allocate(16);
      result.putLong(obj.getMostSignificantBits());
      result.putLong(obj.getLeastSignificantBits());
      result.flip();
      return result;
    }

    private static UUID deserializeUUID(ByteBuffer buffer) {
      long msb = buffer.getLong(0);
      long lsb = buffer.getLong(8);
      return new UUID(msb, lsb);
    }

    private void init(ZMQ.Socket pipe) {
      this.pipe = pipe;
      try {
        udp = new Udplib(PING_PORT_NUMBER);
      } catch (IOException e) {
        e.printStackTrace();
        System.exit(1);
      }
      peers = new ConcurrentHashMap<>();
      uuid = UUID.randomUUID();
      System.out.println("I'm " + uuid);
    }

    private void destroy() throws IOException {
      udp.destroy();
    }

    /**
     * Here we handle the different control messages from the frontend.
     */
    private void controlMsg() {
      //  Get the whole message off the pipe in one go
      ZMsg msg = ZMsg.recvMsg(pipe);
      String command = msg.popString();


      //  We don't actually implement any control commands yet
      //  but if we did, this would be where we did it…
      //  if("EXAMPLE".equals(command)) {
      //  }
      msg.destroy();
    }

    /**
     * This is how we handle a beacon coming into our UDP socket;
     * this may be from other peers or an echo of our own broadcast
     * beacon:
     */
    private void handleBeacon() {
      ByteBuffer uuidBuffer = ByteBuffer.allocate(16);
      try {
        udp.recv(uuidBuffer);
      } catch (IOException e) {
        e.printStackTrace();
        System.exit(1);
      }
      if (Arrays.equals(uuidBuffer.array(), emptyUUID))
        return; //nothing to read
      UUID uuid = deserializeUUID(uuidBuffer);
      //  If we got a UUID and it's not our own beacon, we have a peer
      if (!this.uuid.equals(uuid)) {
        //  Find or create peer via its UUID string
        Peer peer = peers.get(uuid);
        if (peer == null) {
          peer = new Peer(uuid);
          peers.put(uuid, peer);

          //  Report peer joined the network
          pipe.sendMore("JOINED");
          pipe.send(serializeUUID(uuid).array());
        }
        //  Any activity from the peer means it's alive
        peer.isAlive();
      }
    }

    /**
     * This method checks one peer peer for expiration; if the peer hasn't
     * sent us anything by now, it's "dead" and we can delete it:
     */
    private void reapPeer(Peer peer) {
      if (System.currentTimeMillis() >= peer.expiresAt) {
        //  Report peer left the network
        pipe.sendMore("LEFT");
        pipe.send(serializeUUID(peer.uuid).array());
        peers.remove(peer.uuid);
      }
    }

    /**
     * This is the main loop for the background agent. It uses zmq_poll
     * to monitor the frontend pipe (commands from the API) and the
     * backend UDP handle (beacons):
     */
    @Override
    public void run(Object[] args, ZContext ctx, ZMQ.Socket pipe) {
      init(pipe);

      //  Send first beacon immediately
      long pingAt = System.currentTimeMillis();
      ZMQ.PollItem[] pollItems = new ZMQ.PollItem[]{
              new ZMQ.PollItem(pipe, ZMQ.Poller.POLLIN),
              new ZMQ.PollItem(udp.handle(), ZMQ.Poller.POLLIN)
      };
      while (!Thread.currentThread().isInterrupted()) {
        long timeout = pingAt - System.currentTimeMillis();
        if (timeout < 0)
          timeout = 0;
        if (ZMQ.poll(pollItems, 2, timeout) == -1)
          break;              //  Interrupted

        //  If we had activity on the pipe, go handle the control
        //  message. Current code never sends control messages.
        if (pollItems[0].isReadable())
          this.controlMsg();

        //  If we had input on the UDP socket, go process that
        if (pollItems[1].isReadable())
          this.handleBeacon();

        //  If we passed the 1-second mark, broadcast our beacon
        if (System.currentTimeMillis() >= pingAt) {
          try {
            udp.send(serializeUUID(this.uuid));
          } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
          }
          pingAt = System.currentTimeMillis() + PING_INTERVAL;
        }
        //  Delete and report any expired peers
        for (Peer peer : peers.values())
          reapPeer(peer);

      }
      try {
        this.destroy();
      } catch (IOException e) {
        e.printStackTrace();
        System.exit(1);
      }
    }
  }

  /**
   * This class defines each peer that we discover and track:
   */
  private static class Peer {
    private static final int PEER_EXPIRY = 5000;  //  Five seconds and it's gone
    private UUID uuid;          //  Peer's UUID as binary blob
    private long expiresAt;

    private Peer(UUID uuid) {
      this.uuid = uuid;
    }

    private void isAlive() {
      expiresAt = System.currentTimeMillis() + PEER_EXPIRY;
    }
  }
}
