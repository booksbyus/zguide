package ;
import haxe.io.Bytes;
import neko.Lib;
import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQSocket;

/**
 * Pubsub envelope subscriber
 * 
 * See: http://zguide.zeromq.org/page:all#Pub-sub-Message-Envelopes
 * 
 * Use with PSEnvPub
 */
class PSEnvSub 
{

    public static function main() {
        var context:ZMQContext = ZMQContext.instance();
        
        Lib.println("** PSEnvSub (see: http://zguide.zeromq.org/page:all#Pub-sub-Message-Envelopes)");
        
        var subscriber:ZMQSocket = context.socket(ZMQ_SUB);
        subscriber.connect("tcp://127.0.0.1:5563");
        subscriber.setsockopt(ZMQ_SUBSCRIBE, Bytes.ofString("B"));
        
        while (true) {
            var msgAddress:Bytes = subscriber.recvMsg();
            // Read message contents
            var msgContent:Bytes = subscriber.recvMsg();
            trace (msgAddress.toString() + " " + msgContent.toString() + "\n");
        }
        // We never get here but clean up anyway
        subscriber.close();
        context.term();
    }
}