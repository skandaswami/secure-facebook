/**
  * Created by Shiva K on 11/23/2015.
  */

package fbserver

import java.util.concurrent.ConcurrentHashMap

import akka.actor._
import akka.actor.{ActorSystem, Props}
import akka.actor.ActorContext
import akka.actor.ActorSelection
import akka.pattern.ask
import akka.util.Timeout
import spray.can.Http
import spray.http._
import spray.can.server.Stats
import spray.httpx.marshalling._
import spray.httpx.unmarshalling._
import spray.json.DefaultJsonProtocol
import spray.httpx.SprayJsonSupport
import spray.routing._
import MediaTypes._
import scala.concurrent.Future
import scala.concurrent.duration._
import java.io._
import spray.http.{HttpData, MultipartFormData}
import scala.collection.mutable.ListBuffer

case object DisplayStats

class HttpHandler extends Actor with HttpService with Backend {
  //import context._
  implicit val timeout: Timeout = 1.second
  import context.dispatcher
  import JsonSupport._
  import ServerStats._

  var httpListener : Option[ActorRef] = None

  def actorRefFactory = context

  def httpReceive = runRoute(restAPI)

  def handle : Receive = {
    case Http.Bound(_) =>
      httpListener = Some(sender)
    case Http.Unbound =>
      context.stop(self)
  }

  def receive = handle orElse httpReceive

  val restAPI = {

    path("newUserRegistration") {
      pathEndOrSingleSlash {
        httpRequestsProcessed += 1
        post {
          entity(as[UserDataStructure]) { userData =>
            complete {
              //println("Created New User: "+userData.userName)
              registerNewUser(userData)
            }
          }
        }
      }
    } ~
    pathPrefix("login") { headerValueByName("CLIENT_USER_ID") { requesterID =>
      pathEndOrSingleSlash {
        get {
          respondWithMediaType(`application/json`) {
            complete {
              println("PATH: User logging in")
              loginStep1(requesterID)
            }
          }
        }
      } ~
      path("verify") {
        post {
          entity(as[VerifyCredentials]) { verifyCredentials =>
            respondWithMediaType(`application/json`) {
              complete {
                println("PATH: Verify logging in")
                loginVerify(requesterID,verifyCredentials)
              }
            }
          }
        }
      }
    }} ~ //end of login
    pathPrefix("user") {
      (headerValueByName("CLIENT_USER_ID") &
      headerValueByName("CLIENT_TOKEN")) { (requesterID,token) =>
      httpRequestsProcessed += 1
      //val token = "x"
      if(!authenticateUser(requesterID,token)) {
        respondWithMediaType(`application/json`) {
          complete {
            println(s"Authentication failed for user $requesterID")
            s"Authentication failed for user $requesterID"
          }
        }
      } else {
      path("createProfile") {
        post {
          entity(as[EncryptedUserProfile]) { userProfile =>
            respondWithMediaType(`application/json`) {
              complete {
                println("PATH: Creating profile " + requesterID)
                updateProfile(requesterID,userProfile)
              }
            }
          }
        }
      } ~
      path("logout") {
        get {
          complete {
            println("PATH: Logging out user " + requesterID)
            logout(requesterID)
          }
        }
      } ~
      pathPrefix(Segment) { userID =>
        path("sendFriendRequest") {
          get {
            respondWithMediaType(`application/json`) {
              complete {
                addFriendship(userID,requesterID)
              }
            }
          }
        } ~
        pathPrefix("timeLine") {
          pathEnd {
            get {
              respondWithMediaType(`application/json`) {
                complete {
                  println("PATH: Fetching timeline " + requesterID)
                  if(isAllowedToAccess(requesterID,userID)) {
                    serveTimeLine(userID)
                  } else {
                    "Permission Denied"
                  }
                }
              }
            }
          } ~
          path("sharePost") {
            post {
              entity(as[SharedPost]) { sharedPost =>
                respondWithMediaType(`application/json`) {
                  complete {
                    println("PATH: Sharing post with friends " + requesterID)
                    if(isAllowedToAccess(requesterID,userID)) {
                      sharePost(userID,sharedPost)
                    } else {
                      "Permission Denied"
                    }
                  }
                }
              }
            }
          }
        } ~
        path("profile") {
          get {
            respondWithMediaType(`application/json`) {
              complete {
                println("PATH: Fetching profile for " + userID)
                if(isSameUser(requesterID,userID)) {
                  serveUserProfile(userID)
                } else if(isAllowedToAccess(requesterID,userID)) {
                  queueProfileRequest(requesterID,userID)
                } else {
                  "Permission Denied"
                }
              }
            }
          }
        } ~
        path("friends") {
          get {
            respondWithMediaType(`application/json`) {
              complete {
                println("PATH: Fethcing friend list " + userID)
                if(isAllowedToAccess(requesterID,userID)) {
                  serveFriendList(userID,true)
                } else {
                  "Permission Denied"
                }
              }
            }
          }
        } ~
        path("viewFriends") {
          get {
            respondWithMediaType(`application/json`) {
              complete {
                println("PATH: Fethcing friendlist" + userID)
                if(isAllowedToAccess(requesterID,userID)) {
                  serveFriendList(userID,false)
                } else {
                  "Permission Denied"
                }
              }
            }
          }
        } ~
        path("newsFeed") {
          get {
            respondWithMediaType(`application/json`) {
              complete {
                println("PATH: Fethcing newsfeed " + userID)
                if(isSameUser(requesterID,userID)) {
                  serveNewsFeed(userID)
                } else {
                  "Permission Denied"
                }
              }
            }
          }
        } ~
        path("photoStream") {
          get {
            respondWithMediaType(`application/json`) {
              complete {
                println("PATH: Fethcing photostream " + userID)
                if(isSameUser(requesterID,userID)) {
                  servePhotoStream(userID)
                } else {
                  "Permission Denied"
                }
              }
            }
          }
        } ~
        pathPrefix("albums") {
          pathEndOrSingleSlash {
            get {
              respondWithMediaType(`application/json`) {
                complete {
                  println("PATH: Fethcing albumlist " + userID)
                  if(isAllowedToAccess(requesterID,userID)) {
                    serveAlbumList(userID)
                  } else {
                    "Permission Denied"
                  }
                }
              }
            }
          } ~
          path("createAlbum") {
            pathEndOrSingleSlash {
              post {
                entity(as[AlbumData]) { albumData =>
                  //println("IN ALBUM PHOTO" + requesterID+albumName)
                  respondWithMediaType(`application/json`) {
                    complete {
                      println("PATH: Creating an album " + userID)
                      if(isSameUser(requesterID,userID)) {
                        createAlbum(userID,albumData)
                      } else {
                        "Permission Denied"
                      }
                    }
                  }
                }
              }
            }
          } ~
          pathPrefix(Segment) { albumName =>
            pathEndOrSingleSlash {
              get {
                respondWithMediaType(`application/json`) {
                  complete {
                    println("PATH: Fethcing album " + userID)
                    if(isAllowedToAccess(requesterID,userID)) {
                      servePhotoAlbum(userID,albumName)
                    } else {
                      "Permission Denied"
                    }
                  }
                }
              }
            } ~
            path("sharePhoto") {
              pathEndOrSingleSlash {
                post {
                  entity(as[PhotoData]) { photoPost =>
                    //println("IN ALBUM PHOTO" + requesterID+albumName)
                    respondWithMediaType(`application/json`) {
                      complete {
                        println("PATH: Sharing photo " + userID)
                        if(isSameUser(requesterID,userID)) {
                          sharePhoto(userID,albumName,photoPost)
                        } else {
                          "Permission Denied"
                        }
                      }
                    }
                  }
                }
              }
            } ~
            path("shareAlbum") {
              pathEndOrSingleSlash {
                post {
                  entity(as[AlbumData]) { albumData =>
                    //println("IN ALBUM PHOTO" + requesterID+albumName)
                    respondWithMediaType(`application/json`) {
                      complete {
                        println("PATH: Sharing album " + userID)
                        if(isSameUser(requesterID,userID)) {
                          createAlbum(requesterID,albumData)
                        } else {
                          "Permission Denied"
                        }
                      }
                    }
                  }
                }
              }
            } ~
            pathPrefix(Segment) { photoID =>
              pathEndOrSingleSlash {
                get {
                  respondWithMediaType(`application/json`) {
                    complete {
                      println("PATH: Fethcing a photo " + userID)
                      if(isAllowedToAccess(requesterID,userID)) {
                        servePhoto(userID,requesterID,photoID)
                      } else {
                        "Permission Denied"
                      }
                    }
                  }
                }
              }
            } // end of <photo_id segment>
          } //end of <albumName segment>
        } // end of 'albums' prefix
      } // end of prefix user/segment
    } //end of else authenticate
    } //end of header
    } //end of /user
  } // end of restApi

} // end of HttpHandler


class StatActor extends Actor {

  import context.dispatcher
  import ServerStats._

  val stat_interval = 2
  val file = new File("stats.txt")
  if (file.exists()) {
      file.delete()
  }
  file.createNewFile()

  val fw1 = new FileWriter(file, true)
  fw1.write("Uptime,Server Requests,Requests/Sec,NewsFeed,Posts,Friendships,Photos")
  fw1.write("\r\n")
  fw1.close()

  context.system.scheduler.schedule(30 seconds,stat_interval seconds,self,DisplayStats)

  def receive = {
    case DisplayStats =>
            upTime = upTime + stat_interval
            var req = httpRequestsProcessed
            var diff = (req - prevRequests)/stat_interval
            prevRequests = req

            val fw = new FileWriter(file, true)
            fw.write(upTime+","+req+","+diff+","+totalNewsFeedRequests+","+totalPosts+","+totalFriendships+","+totalPhotos)
            fw.write("\r\n")
            fw.close()

            println("=======================================")
            println("=========SERVER STATS==================")
            println("Server uptime: " + upTime)
            println("Server requests so far: " + req)
            println("Server requestsPerSec: " + diff + "req/sec")
            println("=======================================")

    case _ => {}
  }
}
