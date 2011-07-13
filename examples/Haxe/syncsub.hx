package ;

import neko.Lib;
import haxe.io.Bytes;
import neko.Sys;
import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQSocket;

/**
 * Synchronised subscriber
 * 
 * See: http://zguide.zeromq.org/page:all#Node-Coordination 
 * 
 * Use with SyncPub.hx
 */
class SyncSub 
{

    public static function main() {
        var context:ZMQContext = ZMQContext.instance();
        
        Lib.println("** SyncSub (see: http://zguide.zeromq.org/page:all#Node-Coordination)");
        
        // First connect our subscriber socket
        var subscriber:ZMQSocket = context.socket(ZMQ_SUB);
        subscriber.connect("tcp://127.0.0.1:5561");
        subscriber.setsockopt(ZMQ_SUBSCRIBE, Bytes.ofString(""));
        
        // 0MQ is so fast, we need to wait a little while
        Sys.sleep(1.0);
        
        // Second, synchronise with publisher
        var syncClient:ZMQSocket = context.socket(ZMQ_REQ);
        syncClient.connect("tcp://127.0.0.1:5562");
        
        // Send a synchronisation request
        syncClient.sendMsg(Bytes.ofString(""));
        
        // Wait for a synchronisation reply
        var msgBytes:Bytes = syncClient.recvMsg();
        
        // Third, get our updates and report how many we got
        var update_nbr = 0;
        while (true) {
            msgBytes = subscriber.recvMsg();
            if (msgBytes.toString() == "END") {
                break;
            }
            msgBytes = null;
            update_nbr++;
        }
        Lib.println("Received " + update_nbr + " updates\n");
        
        subscriber.close();
        syncClient.close();
        context.term();
    }
}