package ;
import neko.Lib;
import haxe.io.Bytes;
import neko.Sys;
import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQSocket;

/**
 * Publisher for durable but cynical subscriber
 * 
 * See: http://zguide.zeromq.org/page:all#-Semi-Durable-Subscribers-and-High-Water-Marks
 * 
 * Use with DuraSub.hx
 */
class DuraPub2 
{

    public static function main() {
        var context:ZMQContext = ZMQContext.instance();
        
        Lib.println("** DuraPub2 (see: http://zguide.zeromq.org/page:all#-Semi-Durable-Subscribers-and-High-Water-Marks)");
        
        // Subscriber tells us when it is ready here
        var sync:ZMQSocket = context.socket(ZMQ_PULL);
        sync.bind("tcp://*:5564");
        
        // We send updates via this socket
        var publisher:ZMQSocket = context.socket(ZMQ_PUB);
        publisher.bind("tcp://*:5565");
        
        // Prevent publisher overflowing because of slow subscribers
        publisher.setsockopt(ZMQ_HWM, { hi:0, lo: 1 } );   // Set HWM to 1
        
        // Specify swap space in bytes, this covers all subscribers
        publisher.setsockopt(ZMQ_SWAP, { hi:0, lo: 25000000 } );
        
        // Wait for synchronisation request
        sync.recvMsg();
        
        for (update_nbr in 0 ... 10) {
            var str = "Update " + update_nbr;
            Lib.println(str);
            publisher.sendMsg(Bytes.ofString(str));
            Sys.sleep(1.0);
        }
        publisher.sendMsg(Bytes.ofString("END"));
        
        sync.close();
        publisher.close();
        context.term();
    }
}