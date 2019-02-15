import org.zeromq.ZMsg;

/**
 * UDP ping command
 * Model 3, uses abstract network interface
 */
public class Udpping3 {
  public static void main(String[] args) {
    Interface interfaceT = new Interface();
    while (true) {
      ZMsg msg = interfaceT.recv();
      if (msg == null)
        break;  //  Interrupted
      msg.dump();
    }
    interfaceT.destroy();
  }
}
