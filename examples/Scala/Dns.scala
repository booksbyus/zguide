/*
 *  Micro DNS for resolving connection endpoints
 *
 *
 *
 * @Author:     Giovanni Ruggiero
 * @Email:      giovanni.ruggiero@gmail.com
*/

class Dns(hosts: Map[String,(String,Int)], ports: Map[String,String]) {
	private def getHost(hostName: String) = hosts(hostName)._1

	def endPoint(host: String, port: String) =  "tcp://" + getHost(host) + ":" + ports(port) + hosts(host)._2

	def bind(host: String, port: String) = "tcp://*:" + ports(port) + hosts(host)._2

}

object ClusterDns {

	private val hosts = Map(
		"DC1"  -> ("localhost", 1),
		"DC2"  -> ("localhost", 2),
		"DC3"  -> ("localhost", 3)
	)

	val ports = Map(
		"statefe" -> "5550",
		"statebe" -> "5550",
		"localfe" -> "5551",
		"localbe" -> "5552",
		"cloudfe" -> "5553",
		"cloudbe" -> "5553"
	)

	val clusterDns = new Dns(hosts,ports)

}

