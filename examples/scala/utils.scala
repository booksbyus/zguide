/*
 * Utility methods
 *
 * @author Giovanni Ruggiero
 * @email giovanni.ruggiero@gmail.com
*/

import org.zeromq.ZMQ.Socket
import org.zeromq.ZMQ
import java.util.Random

object ZHelpers {

  private var rand: Random = null

  private def GetRandomGen() = {
    if (rand == null)
      rand = new Random(System.currentTimeMillis)
    rand
  }

  private def decodeUUID(data: Array[Byte]) = {
		println(new String(data))
		val hex = "0123456789ABCDEF"
		val uuid = new Array[Char](33)
    uuid(0) = '@'
		for (byteNbr <- 0 until 16)  {
			println(byteNbr)
			println(data(byteNbr + 1))
			println(data(byteNbr + 1) >> 4)
			uuid(byteNbr * 2 + 1) = hex(data(byteNbr + 1) >> 4)
			uuid(byteNbr * 2 + 2) = hex(data(byteNbr + 1) & 15)
    }
    new String(uuid)
  }

	implicit def jSocket2cSocket(socket: Socket) = new pmpdSocket(socket)

  def setID(socket: Socket) {
		val rand = GetRandomGen
		socket.setIdentity((rand.nextLong.toString + "-" + rand.nextLong.toString).getBytes)
  }

  def dump(socket: Socket) = {
    println(new String("-") * 38)
		for (msg <- socket.recvAll(0)) {
			printf("[%d] ",msg.length)
 			if (msg.length == 17 && msg(0) == 0)  {
        // println(decodeUUID(msg).substring(1))
        println( "UUID " + new String(msg))
      } else {
        println(new String(msg))
      }
		}
  }

	class pmpdSocket(socket: Socket) {
		def recvAll(flags: Int) = {
			val messages = scala.collection.mutable.Queue[Array[Byte]]()
			messages.enqueue(socket.recv(flags))
			while (socket.hasReceiveMore) {
				messages.enqueue(socket.recv(flags))
			}
			messages
		}

		def sendMsg(msg: ZMsg) =  msg send socket

		def recvMsg() = new ZMsg(socket)

		def dnsBind(port: String)(implicit host: String, dns: Dns) = socket.bind(dns.bind(host, port))
		def dnsConnect(host: String, port: String)(implicit dns: Dns) = socket.connect(dns.endPoint(host, port))
	}
}
