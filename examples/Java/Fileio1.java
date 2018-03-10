import org.zeromq.*;
import org.zeromq.ZMQ.Socket;
import zmq.ZError;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Arrays;

//  File Transfer model #1
//
//  In which the server sends the entire file to the client in
//  large chunks with no attempt at flow control.
public class Fileio1 {
  private static final int CHUNK_SIZE = 250000;

  //  The main task starts the client and server threads; it's easier
  //  to test this as a single process with threads, than as multiple
  //  processes:
  public static void main(String[] args) {
    ZContext ctx = new ZContext();
    //  Start child threads
    ZThread.fork(ctx, new Server());
    Socket client = ZThread.fork(ctx, new Client());
    //  Loop until client tells us it's done
    client.recvStr();
    //  Kill server thread
    ctx.destroy();
  }

  static class Client implements ZThread.IAttachedRunnable {

    @Override
    public void run(Object[] objects, ZContext zContext, Socket pipe) {

      Socket dealer = zContext.createSocket(ZMQ.DEALER);
      dealer.connect("tcp://127.0.0.1:6000");
      dealer.send("fetch");
      long total = 0;   //  Total bytes received
      long chunks = 0;  //  Total chunks received

      while (true) {
        ZFrame frame = ZFrame.recvFrame(dealer);
        chunks++;
        long size = frame.size();
        frame.destroy();
        total += size;
        if (size == 0)
          break;   //  Whole file received
      }
      System.out.printf("%d chunks received, %d bytes\n", chunks, total);
      pipe.send("OK");
    }
  }
  //  The server thread reads the file from disk in chunks, and sends
  //  each chunk to the client as a separate message. We only have one
  //  test file, so open that once and then serve it out as needed:
  static class Server implements ZThread.IAttachedRunnable {

    @Override
    public void run(Object[] objects, ZContext zContext, Socket socket) {
      File file = new File("testdata");
      FileInputStream fr;
      try {
        fr = new FileInputStream(file);
      } catch (FileNotFoundException e) {
        e.printStackTrace();
        return;
      }
      Socket router = zContext.createSocket(ZMQ.ROUTER);
      //  Default HWM is 1000, which will drop messages here
      //  because we send more than 1,000 chunks of test data,
      //  so set an infinite HWM as a simple, stupid solution:
      router.setHWM(0);
      router.bind("tcp://*:6000");
      while (!Thread.currentThread().isInterrupted()) {
        //  First frame in each message is the sender identity
        ZFrame identity;
        try {
          identity = ZFrame.recvFrame(router);
        } catch (ZMQException e) {
          if (e.getErrorCode() == ZError.ETERM)
            break;
          e.printStackTrace();
          break;
        }

        //  Second frame is "fetch" command
        String command = router.recvStr();
        assert ("fetch".equals(command));
        byte[] data = new byte[CHUNK_SIZE];
        int size;
        while (true) {
          try {
            if (fr.available() <= CHUNK_SIZE)
              size = fr.read(data);
            else
              size = fr.read(data, 0, CHUNK_SIZE);
          } catch (IOException e) {
            e.printStackTrace();
            break;
          }
          ZFrame chunk = new ZFrame(Arrays.copyOf(data, size < 0 ? 0 : size));
          identity.send(router, ZMQ.SNDMORE);
          chunk.sendAndDestroy(router, 0);
          if (size <= 0)
            break;    //  Always end with a zero-size frame
        }
        identity.destroy();
      }
      try {
        fr.close();
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
  }
}
