// Demonstrate identities as used by the request-reply pattern.
//
// @author Giovanni Ruggiero
// @email giovanni.ruggiero@gmail.com

import org.zeromq.ZMQ
import ZHelpers._

object identity {
  def main(args : Array[String]) {
	val context = ZMQ.context(1)

	val sink = context.socket(ZMQ.DEALER)
	sink.bind("inproc://example")

	val anonymous = context.socket(ZMQ.REQ)
	anonymous.connect("inproc://example")
	anonymous.send("ROUTER uses a generated 5 byte identity".getBytes,0)
	dump(sink)

	val identified = context.socket(ZMQ.REQ)
	identified.setIdentity("PEER2" getBytes)
	identified.connect("inproc://example")
	identified.send("ROUTER socket uses REQ's socket identity".getBytes,0)
	dump(sink)

	identified.close
    }
}
