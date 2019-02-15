import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZThread;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Arrays;

//  File Transfer model #2
//
//  In which the client requests each chunk individually, thus
//  eliminating server queue overflows, but at a cost in speed.
public class Fileio2 {
  private static final int CHUNK_SIZE = 250000;

  //  The main task is just the same as in the first model.
  public static void main(String[] args) {
    ZContext ctx = new ZContext();
    //  Start child threads
    ZThread.fork(ctx, new Fileio1.Server());
    ZMQ.Socket client = ZThread.fork(ctx, new Fileio1.Client());
    //  Loop until client tells us it's done
    client.recvStr();
    //  Kill server thread
    ctx.destroy();
  }
  static class Client implements ZThread.IAttachedRunnable {

    @Override
    public void run(Object[] objects, ZContext zContext, ZMQ.Socket pipe) {
      ZMQ.Socket dealer = zContext.createSocket(ZMQ.DEALER);
      dealer.setHWM(1);
      dealer.connect("tcp://127.0.0.1:6000");
      long total = 0;//  Total bytes received
      long chunks = 0;//  Total chunks received

      while (true) {
        //  Ask for next chunk
        dealer.sendMore("fetch");
        dealer.sendMore(String.valueOf(total));
        dealer.send(String.valueOf(CHUNK_SIZE));

        ZFrame chunk = ZFrame.recvFrame(dealer);
        if (chunk.getData() == null)
          break;//  Shutting down, quit
        chunks++;
        long size = chunk.size();
        chunk.destroy();
        total += size;
        if (size < CHUNK_SIZE)
          break;              //  Last chunk received; exit
      }
      System.out.printf("%d chunks received, %d bytes\n", chunks, total);
      pipe.send("OK");
    }
  }

  //  The server thread waits for a chunk request from a client,
  //  reads that chunk, and sends it back to the client:
  static class Server implements ZThread.IAttachedRunnable {

    @Override
    public void run(Object[] objects, ZContext zContext, ZMQ.Socket socket) {
      File file = new File("testdata");
      FileInputStream fr;
      try {
        fr = new FileInputStream(file);
      } catch (FileNotFoundException e) {
        e.printStackTrace();
        return;
      }
      ZMQ.Socket router = zContext.createSocket(ZMQ.ROUTER);
      router.setHWM(1);
      router.bind("tcp://*:6000");

      while (!Thread.currentThread().isInterrupted()) {
        //  First frame in each message is the sender identity
        ZFrame identity = ZFrame.recvFrame(router);
        if (identity.getData() == null)
          break; //  Shutting down, quit

        //  Second frame is "fetch" command
        String command = router.recvStr();
        assert ("fetch".equals(command));

        //  Third frame is chunk offset in file
        int offset = Integer.parseInt(router.recvStr());

        //  Fourth frame is maximum chunk size
        int chunkSize = Integer.parseInt(router.recvStr());

        //  Read chunk of data from file
        byte[] data = new byte[CHUNK_SIZE];
        int size;
        try {
          fr.skip(offset);
        size = fr.read(data,0,chunkSize);
        } catch (IOException e) {
          e.printStackTrace();
          break;
        }

        //  Send resulting chunk to client
        ZFrame chunk = new ZFrame(Arrays.copyOf(data, size < 0 ? 0 : size));
        identity.sendAndDestroy(router,ZMQ.SNDMORE);
        chunk.sendAndDestroy(router);
      }
      try {
        fr.close();
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
  }
}
