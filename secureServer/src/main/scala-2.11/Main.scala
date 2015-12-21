/**
  * Created by Shiva K on 11/24/2015.
  */

package fbserver

import java.util.concurrent.ConcurrentHashMap
import scala.collection.concurrent
import scala.collection.convert.decorateAsScala.mapAsScalaConcurrentMapConverter
import com.typesafe.config.ConfigFactory
import akka.actor.{ActorRef,ActorSystem,Props}
import akka.actor.actorRef2Scala
import akka.io.IO
import akka.util.Timeout
import spray.can.Http

object Server {

  def main(args: Array[String]) {

  implicit val system = ActorSystem("Server")

    //val serverAddress= "192.168.0.24"
    //val serverAddress= "192.168.0.24"
  val no_of_ports = 1
  val base_port = 7200
  val serverAddress= java.net.InetAddress.getLocalHost.getHostAddress()
  //val serverAddress= "127.0.0.1"

  for (i <- 0 to (no_of_ports-1)) {
    val httpHandlerActor = system.actorOf(Props(new HttpHandler()),name = "HttpHandler"+i)
    var po = base_port + i
    IO(Http).tell(Http.Bind(httpHandlerActor, interface = serverAddress, port = po), sender = httpHandlerActor)
  }

  //val statActor = system.actorOf(Props(new StatActor()),name = "StatActor")
  }
}
