import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQException;
import zmq.ZError;

/**
 * Shows how to provoke EAGAIN when reaching HWM
 */
public class Eagain {
  public static void main(String[] args) {
    ZContext ctx = new ZContext();
    ZMQ.Socket mailbox = ctx.createSocket(ZMQ.DEALER);
    mailbox.setSndHWM(4);
    mailbox.setSendTimeOut(0);
    mailbox.connect("tcp://localhost:9876");

    int count;
    for (count = 0; count < 10; count++) {
      System.out.printf("Sending message %d\n", count);
      try {
        mailbox.send(String.format("message %d", count));
        if (mailbox.base().errno() == ZError.EAGAIN) {
          System.out.printf("error code: EAGAIN\n");
          break;
        }
      } catch (ZMQException e) {
        System.out.printf("error code: %s", ZError.toString(e.getErrorCode()));
        break;
      }
    }
    ctx.destroy();
  }
}
