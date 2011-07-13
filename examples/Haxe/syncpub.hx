package ;
import haxe.io.Bytes;
import neko.Lib;
import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQSocket;

/**
 * Synchronised publisher
 * 
 * See: http://zguide.zeromq.org/page:all#Node-Coordination
 * 
 * Use with SyncSub.hx
 */
class SyncPub 
{
    static inline var SUBSCRIBERS_EXPECTED = 10;
    
    public static function main() {
        var context:ZMQContext = ZMQContext.instance();
        Lib.println("** SyncPub (see: http://zguide.zeromq.org/page:all#Node-Coordination)");
        
        // Socket to talk to clients
        var publisher:ZMQSocket = context.socket(ZMQ_PUB);
        publisher.bind("tcp://*:5561");
        
        // Socket to receive signals
        var syncService:ZMQSocket = context.socket(ZMQ_REP);
        syncService.bind("tcp://*:5562");
        
        // get synchronisation from subscribers
        var subscribers = 0;
        while (subscribers < SUBSCRIBERS_EXPECTED) {
            // wait for synchronisation request
            var msgBytes = syncService.recvMsg();
            
            // send synchronisation reply
            syncService.sendMsg(Bytes.ofString(""));
            subscribers++;
        }
        
        // Now broadcast exactly 1m updates followed by END
        for (update_nbr in 0 ... 1000000) {
            publisher.sendMsg(Bytes.ofString("Rhubarb"));
        }
        publisher.sendMsg(Bytes.ofString("END"));
        
        publisher.close();
        syncService.close();
        context.term();
    }
}