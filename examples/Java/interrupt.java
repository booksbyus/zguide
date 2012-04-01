/*
 *
 *  Interrupt in Java
 *  Shows how to handle Ctrl-C
 *
 * @author Vadim Shalts
 * @email vshalts@gmail.com
*/

import org.zeromq.ZMQ;
import org.zeromq.ZMQException;

public class interrupt {
   public static void main(String[] args) {
      //  Prepare our context and socket
      final ZMQ.Context context = ZMQ.context(1);

      final Thread zmqThread = new Thread() {
         @Override
         public void run() {
            ZMQ.Socket socket = context.socket(ZMQ.REP);
            socket.bind("tcp://*:5555");

            while (!Thread.currentThread().isInterrupted()) {
               try {
                  socket.recv(0);
               } catch (ZMQException e) {
                  // context destroyed, exit
                  if (ZMQ.Error.ETERM.getCode() == e.getErrorCode()) {
                     break;
                  }
                  throw e;
               }
            }

            socket.close();
            System.out.println("ZMQ socket shutdown complete");
         }
      };

      Runtime.getRuntime().addShutdownHook(new Thread() {
         @Override
         public void run() {
            System.out.println("ShutdownHook called");
            context.term();
            try {
               zmqThread.interrupt();
               zmqThread.join();
            } catch (InterruptedException e) {
            }
         }
      });

      zmqThread.start();
   }
}