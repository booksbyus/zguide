/*
 * Multipart message class for example applications.
 *
 * @Author:     Giovanni Ruggiero
 * @Email:      giovanni.ruggiero@gmail.com
*/

import org.zeromq.ZMQ.Socket
import org.zeromq.ZMQ
import ZHelpers._

class ZMsg {
	type ZMsg = scala.collection.mutable.ListBuffer[Array[Byte]]
	private val msgParts = scala.collection.mutable.ListBuffer[Array[Byte]]()

	private val Noflags = 0

	def this(body: String) = { this(); msgParts += body.getBytes}

	def this(body: Array[Byte]) = this(new String(body))

	def this(socket: Socket) = {this(); recv(socket)}

	def body = msgParts last

	def body(bodyStr: String) = msgParts.update(msgParts.length -1,bodyStr.getBytes)


	def address = msgParts head

	def addressToString = new String(msgParts head)

	def address(address:String) = {
		msgParts remove 0
		address.getBytes +: msgParts
	}

	def recv(socket: Socket) =  for (frame <- socket.recvAll(0)) msgParts += frame

	def send(socket: Socket) = {
		val last = msgParts last

		for (frame <- msgParts if frame != last) {
			socket.send(frame,ZMQ.SNDMORE)
		}
		socket.send(last,Noflags)
	}

	def length = msgParts.length

	def stringToBody(bodyStr: String) = body(bodyStr)

	def bodyToString = new String(body)

	def wrap(address:Array[Byte]) = {
		Array[Byte]() +: msgParts
		address +: msgParts
	}

	def unwrap() = {
		val address = msgParts remove 0

		msgParts remove 0

		address
	}

	override def toString = msgParts.mkString("|","|","|")

}
