package ;
import haxe.io.Bytes;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQSocket;
import org.zeromq.ZFrame;


/**
 * Custom routing Router to Papa (ROUTER to REP)
 * 
 * We will do this all in one thread to emphasize the sequence
 * of events...
 * 
 * See: http://zguide.zeromq.org/page:all#Address-based-Routing
 */
class RTPapa 
{

	public static function main() {
		Lib.println("** RTPapa (see: http://zguide.zeromq.org/page:all#Address-based-Routing)");
		
		var context:ZContext = new ZContext();
		var client:ZMQSocket = context.createSocket(ZMQ_ROUTER);
		client.bind("ipc:///tmp/routing.ipc");
		var worker:ZMQSocket = context.createSocket(ZMQ_REP);
		worker.setsockopt(ZMQ_IDENTITY, Bytes.ofString("A"));
		worker.connect("ipc:///tmp/routing.ipc");
		
		// Wait for the worker to connect so that when we send a message
		// with a routing envelope, it will actually match the worker...
		Sys.sleep(1);
		
		// Send papa address, address stack, empty part, and request
		ZFrame.newStringFrame("A").send(client, ZFrame.ZFRAME_MORE);
		ZFrame.newStringFrame("address 3").send(client, ZFrame.ZFRAME_MORE);
		ZFrame.newStringFrame("address 2").send(client, ZFrame.ZFRAME_MORE);
		ZFrame.newStringFrame("address 1").send(client, ZFrame.ZFRAME_MORE);
		ZFrame.newStringFrame("").send(client, ZFrame.ZFRAME_MORE);
		ZFrame.newStringFrame("This is the workload").send(client);
		
		// Worker should just get the workload
		ZHelpers.dump(worker);
		
		// We don't play with the envelopes in the worker
		ZFrame.newStringFrame("This is the reply").send(worker);
		
		// Now we dump what we got off the ROUTER socket
		ZHelpers.dump(client);
		
		context.destroy();
	}
}