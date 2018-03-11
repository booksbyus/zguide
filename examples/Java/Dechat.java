import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQException;
import org.zeromq.ZThread;
import zmq.ZError;

import java.util.Scanner;

/**
 * Decentralized chat example
 */
public class Dechat {
  public static void main(String[] args) {
    if (args.length < 3) {
      System.out.println("Usage: dechat ipaddress interface username");
      System.out.println("Example: dechat 192.168.55.123 localhost joe");
      System.exit(0);
    }
    ZContext ctx = new ZContext();

    //cut string after dot
    String addressWithoutLastPart = args[0].substring(0, args[0].lastIndexOf('.'));
    ZThread.fork(ctx, new ListenerTask(), addressWithoutLastPart);
    ZMQ.Socket broadcaster = ctx.createSocket(ZMQ.PUB);
    broadcaster.bind(String.format("tcp://%s:9000", args[1]));
    Scanner scanner = new Scanner(System.in);
    while (!Thread.currentThread().isInterrupted()) {
      String line = scanner.nextLine();
      if (line.isEmpty())
        break;
      broadcaster.send(String.format("%s: %s", args[2], line));
    }
    ctx.destroy();
  }

  static class ListenerTask implements ZThread.IAttachedRunnable {

    @Override
    public void run(Object[] args, ZContext ctx, ZMQ.Socket pipe) {
      ZMQ.Socket listener = ctx.createSocket(ZMQ.SUB);
      int address;
      for (address = 1; address < 255; address++)
        listener.connect(String.format("tcp://%s.%d:9000", args[0], address));

      listener.subscribe(ZMQ.SUBSCRIPTION_ALL);
      while (!Thread.currentThread().isInterrupted()) {
        String message;
        try {
          message = listener.recvStr();
        } catch (ZMQException e) {
          if (e.getErrorCode() == ZError.ETERM)
            break;
          e.printStackTrace();
          break;
        }
        if (!message.isEmpty())
          System.out.println(message);
      }
    }
  }
}
