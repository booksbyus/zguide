package ;
import haxe.io.Bytes;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQException;
import org.zeromq.ZMQSocket;

/**
 * Pubsub envelope publisher
 * 
 * See: http://zguide.zeromq.org/page:all#Pub-sub-Message-Envelopes
 * 
 * Use with PSEnvSub
 */
class PSEnvPub 
{

    public static function main() {
        var context:ZMQContext = ZMQContext.instance();
        
        Lib.println("** PSEnvPub (see: http://zguide.zeromq.org/page:all#Pub-sub-Message-Envelopes)");
        
        var publisher:ZMQSocket = context.socket(ZMQ_PUB);
        publisher.bind("tcp://*:5563");
        
        ZMQ.catchSignals();
        

        while (true) {
            publisher.sendMsg(Bytes.ofString("A"), SNDMORE);
            publisher.sendMsg(Bytes.ofString("We don't want to see this"));
            publisher.sendMsg(Bytes.ofString("B"), SNDMORE);
            publisher.sendMsg(Bytes.ofString("We would like to see this"));
            Sys.sleep(1.0);
        }
        // We never get here but clean up anyhow
        publisher.close();
        context.term();
    }
}