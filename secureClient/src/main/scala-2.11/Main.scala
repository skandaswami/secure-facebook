
package fbclient

import akka.actor._
object mainglobs
{
  var serverAddress:String=""
}
 object Client
{
  def main(args: Array[String])
  {
    import mainglobs._
    serverAddress=args(0)
    implicit val system = ActorSystem("ClientSystem")
    implicit val UserSpawnerActor= system.actorOf(Props[UserSpawner],name="UserSpawner")
    UserSpawnerActor ! StartSpawning
  }
}
