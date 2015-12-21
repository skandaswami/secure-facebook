


/**
  * Created by Shiva K on 11/25/2015.
  */
package fbclient

import java.math.BigInteger
import java.security._

import scala.concurrent.duration._
import scala.collection.mutable.{Map, ArrayBuffer}
import scala.util.Random
import akka.actor._
import akka.io.IO
import akka.pattern.ask
import akka.util.{Timeout}
import spray.can.Http
import spray.http._
import spray.json._
import MediaTypes._
import spray.http.HttpHeaders._
import spray.http.HttpMethods._
import java.io._
import scala.io.{Source}

import java.security.spec.{X509EncodedKeySpec, InvalidKeySpecException, InvalidParameterSpecException, KeySpec}

import java.util.Base64

//MESSAGES
  //Global variable declarations.
  object Globs
  {
    var serverPublicKey:PublicKey=null
    var port:Integer=7200
    val no_of_clients=30

  }
  //Definition of the User Actor
  class User(userName:String,key:KeyPair) extends Actor with ActorLogging {

    import context._
    import Globs._

    implicit val timeout: Timeout = 300.seconds
    var serverAddress = fbclient.mainglobs.serverAddress + ":" + port
    val privateKey = key.getPrivate()
    val publicKey = key.getPublic()
    var loggedIn:Boolean=false
    var usertokenID:String=""
    var head =List(RawHeader("CLIENT_USER_ID", userName),RawHeader("CLIENT_TOKEN",usertokenID))

    //Registration and Login of a new user.
    override def preStart() = {
      import JsonSupport._
      var newUser = new UserDataStructure(userName,publicKey)
      var simpleRsa= new SimpleRsa()
      for {
      response <- IO(Http).ask(HttpRequest(POST, Uri(s"http://$serverAddress/newUserRegistration"), entity = HttpEntity(`application/json`,newUser.makeJsonString()))).mapTo[HttpResponse]
      } yield {
        //println(response.entity.asString)
        serverPublicKey=KeyFactory.getInstance("RSA").generatePublic(new X509EncodedKeySpec(BigInt(response.entity.asString, 16).toByteArray))
        for {
        response <- IO(Http).ask(HttpRequest(GET, Uri(s"http://$serverAddress/login"),headers = List(RawHeader("CLIENT_USER_ID", userName)))).mapTo[HttpResponse]
        }yield {
          var responseRandom = (response.entity.asString)
          var Rsa = new SimpleRsa()
          var decryptedRandom = Rsa.doRsaDecryption(responseRandom, privateKey)
          var encryptedString = new EncryptedString(Rsa.doSimpleRsaEncryption(decryptedRandom,serverPublicKey))
          for {
            response <- IO(Http).ask(HttpRequest(POST, Uri(s"http://$serverAddress/login/verify"),headers = List(RawHeader("CLIENT_USER_ID", userName)),entity = HttpEntity(`application/json`, encryptedString.makeJsonString()))).mapTo[HttpResponse]
          } yield {
            usertokenID=response.entity.asString
            usertokenID=Rsa.doSimpleRsaEncryption(usertokenID,serverPublicKey)
            head=List(RawHeader("CLIENT_USER_ID", userName),RawHeader("CLIENT_TOKEN",usertokenID))
          }
        }
      }
      system.scheduler.scheduleOnce(5000 millis,self,createProfile())
      system.scheduler.schedule(10000 millis,2000 millis,self,scheduleActions())
    }


    def receive = {

      case scheduleActions() =>{

        var random=new Random()
        //if(random.nextInt(10)%3==0)
         // {
            var i=random.nextInt(16)
            i match {
              case 0 => system.scheduler.scheduleOnce (0 millis,self,sendFriendRequest())
              case 1 => system.scheduler.scheduleOnce (0 millis, self, sharePhoto())
              case 2 => system.scheduler.scheduleOnce(0 millis, self,viewTimeline())
              case 3 => system.scheduler.scheduleOnce(0 millis, self,sendFriendRequest())
              case 4 => system.scheduler.scheduleOnce(0 millis,self,viewTimeline())
              case 5 =>  system.scheduler.scheduleOnce(0 millis,self,createAlbum())
              case 6 => system.scheduler.scheduleOnce(0 millis,self,newsFeed())
              case 7 =>  system.scheduler.scheduleOnce(0 millis,self,sharePhoto())
              case 8 =>  system.scheduler.scheduleOnce(0 millis,self,viewFriendList())
              case 9 =>  system.scheduler.scheduleOnce(0 millis,self,photoStream())
              case 10 =>  system.scheduler.scheduleOnce(0 millis,self,photoStream())
              case 11 =>  system.scheduler.scheduleOnce(0 millis,self,sharePost())
              case 12 =>  system.scheduler.scheduleOnce(0 millis,self,sharePost())
              case 13 =>  system.scheduler.scheduleOnce(0 millis,self,sharePost())
              case 14 => system.scheduler.scheduleOnce(0 millis,self,newsFeed())
              case 15 => system.scheduler.scheduleOnce (0 millis,self,sendFriendRequest())
            }
         // }
      }

      case createProfile() => {
        var passphrase: String = null
        var random = new SecureRandom()
        passphrase = new BigInteger(130, random).toString(32)
        var userProfile = new Profile("Harry Potter", "21", "Male", "Single")
        var (encryptedUserData, encryptedAesKey) = userProfile.Encrypt(publicKey)
        var encryptedProfile = new EncryptedUserProfile(encryptedUserData, encryptedAesKey)
        for {
          response <- IO(Http).ask(HttpRequest(POST, Uri(s"http://$serverAddress/user/createProfile"), headers = head, entity = HttpEntity(`application/json`, encryptedProfile.makeJsonString()))).mapTo[HttpResponse]
        } yield {
          println(response.entity.asString)
        }
      }
      case createPost() => {
        val newPost = Post("Hey! It's Christmas time!, stop working!")
        var (encryptedPostContent,encryptedKey)=newPost.Encrypt(publicKey)
        var encryptPost=new EncryptedPost(encryptedPostContent,encryptedKey)
        for {
          response <- IO(Http).ask(HttpRequest(POST, Uri(s"http://$serverAddress/user/$userName/timeLine/createPost"), headers = List(RawHeader("CLIENT_USER_ID", userName)),entity = HttpEntity(`application/json`, encryptPost.makeJsonString())))
        } yield {
          println("CREATE POST:"+response)
        }
      }
      case sharePost() =>{

        val newPost = Post("Username->"+userName+"-posts-> Hey guys! This post is exclusive to just a few of you!, I handpicked you guys :)")
        var (encryptedPostContent,encryptedKey)=newPost.Encrypt(publicKey)

        var encryptPost=new EncryptedPost(encryptedPostContent,encryptedKey)
        for{
          response <- IO(Http).ask(HttpRequest(GET, Uri(s"http://$serverAddress/user/$userName/friends"),headers =head)).mapTo[HttpResponse]
        } yield {

          var friendList:String=response.entity.asString
          //getEncryptedKeys returns the Aes keys encrypted with the public keys of only the users that the post is meant to be shared to.
          var AccessList:scala.collection.immutable.Map[String,String]=getEncryptedKeys(friendList,encryptedKey)
          //Data structure containing the encrypted posts and the Aes Keys encrypted with the respective user's public keys.
          var encryptedPostListAndAesEncryptedPostContent=new sharedPost(encryptedPostContent,AccessList)

          for {
            response <- IO(Http).ask(HttpRequest(POST, Uri(s"http://$serverAddress/user/$userName/timeLine/sharePost"),  headers =head,entity = HttpEntity(`application/json`,encryptedPostListAndAesEncryptedPostContent.makeJsonString()))).mapTo[HttpResponse]
          } yield {
            println("CREATED POST:"+response.entity.asString)
          }
        }
      }
      case checkNotifications() => {
        for {
          response <- IO(Http).ask(HttpRequest(GET, Uri(s"http://$serverAddress/user/checkNotifications"), headers = List(RawHeader("CLIENT_USER_ID", userName)))).mapTo[HttpResponse]
        } yield {
          println(response.entity.asString)
        }
      }
      case newsFeed() => {
        println("\nUser viewing newsFeed\n")
        for {
          response <- IO(Http).ask(HttpRequest(GET, Uri(s"http://$serverAddress/user/$userName/newsFeed"), headers = head)).mapTo[HttpResponse]
        } yield {
          //println("NEWSFEED:"+response.entity.asString)
          var newsFeedPosts = getPostMap(response.entity.asString)
          println("\nThese are the NewsFeed posts\n")
          newsFeedPosts foreach { t2 =>
            var ePost = new EncryptedPost(t2._1, t2._2)
            var Post = ePost.Decrypt(privateKey)
            Post.print()
          }
        }
      }
      case photoStream() => {
        println("\nUser viewing PhotoStream\n")
        for {
          response <- IO(Http).ask(HttpRequest(GET, Uri(s"http://$serverAddress/user/$userName/photoStream"), headers = head)).mapTo[HttpResponse]
        } yield {
          var PhotoStreamPosts = getPhotoMap(response.entity.asString)
          //println("\nThese are the PhotoStream posts\n" + PhotoStreamPosts)
          PhotoStreamPosts foreach { t2 =>
            var ePhoto = new EncryptedPhoto(t2._1, t2._2)
            var Photo = ePhoto.Decrypt(privateKey)
            Photo.print()
          }
        }
      }
      case sendFriendRequest() => {
        println("\nSending friend Request")
        var random=new Random()
        var friend: String = "user"+random.nextInt(5)
        for {
          response <- IO(Http).ask(HttpRequest(GET, Uri(s"http://$serverAddress/user/$friend/sendFriendRequest"), headers =head)).mapTo[HttpResponse]
        } yield {
          println("SENT FRIEND REQUEST:" + response.entity.asString)
        }
      }
      case viewTimeline()=>{
        println("\nViewing Timeline\n")
        for {
          response <- IO(Http).ask(HttpRequest(GET, Uri(s"http://$serverAddress/user/$userName/timeLine"), headers =head)).mapTo[HttpResponse]
        } yield {
           var timeLinePosts=getPostMap(response.entity.asString)
           //println("\nTIMELINE POST LIST\n"+timeLinePosts)
           timeLinePosts foreach{t2=>
            var ePost=new EncryptedPost(t2._1,t2._2)
            var Post=ePost.Decrypt(privateKey)
             Post.print()
           }
        }
      }
      case viewProfile() => {
        for {
          response <- IO(Http).ask(HttpRequest(GET, Uri(s"http://$serverAddress/user/$userName/profile"), headers =head)).mapTo[HttpResponse]
        } yield {
          println("VIEW PROFILE:" + response.entity.asString)
        }
      }
      case viewFriendList() => {
        println("\n Viewing friend List")
        var friendNames:String = ""

        for{
          response <- IO(Http).ask(HttpRequest(GET, Uri(s"http://$serverAddress/user/$userName/friends"),headers = head)).mapTo[HttpResponse]
        } yield {
          var friendList:String=response.entity.asString
          var friends=friendList.split(",")
          var flist = new ArrayBuffer[String]()
          for(i <-0 until friends.size)
            {
              var friendNamesAndKeys=friends(i).split(":")
              flist+=friendNamesAndKeys(0)
            }
          println("\nList of Friends:"+flist+"\n")
        }
      }
      case sharePhoto() => {
        println("In Share photo")
        var random = new Random()
        var albumid = "album" + random.nextInt(3)
        val file = "image.jpg"
        val bis = new BufferedInputStream(new FileInputStream(file))
        val bArray = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
        val bArrayString=java.util.Base64.getEncoder.encodeToString(bArray)
        var newPhoto = new Photo(albumid, bArrayString)
        var (encryptedPhotoData, encryptedKey) = newPhoto.Encrypt(publicKey)
        for {
          response <- IO(Http).ask(HttpRequest(GET, Uri(s"http://$serverAddress/user/$userName/friends"), headers = head)).mapTo[HttpResponse]
        } yield {
          var friendList: String = response.entity.asString
          var AccessList: scala.collection.immutable.Map[String, String] = getEncryptedKeys(friendList, encryptedKey)
          for {
            response <- IO(Http).ask(HttpRequest(GET, Uri(s"http://$serverAddress/user/$userName/albums"), headers = head)).mapTo[HttpResponse]
          } yield {
            var albumList = response.entity.asString
            var albumNames = albumList.split(",")
            var album = albumNames(random.nextInt(albumNames.size))
            var photoShared=new sharedPhoto(encryptedPhotoData,AccessList)

            for {
              response <- IO(Http).ask(HttpRequest(POST, Uri(s"http://$serverAddress/user/$userName/albums/$album/sharePhoto"), entity = HttpEntity(`application/json`, photoShared.makeJsonString()), headers =head)).mapTo[HttpResponse]
            } yield {
              println("SHARED PHOTO:" + response.entity.asString)
            }
          }
        }
      }
      case createAlbum() =>{
        val random=new Random()
        var albumName=new AlbumName("album"+random.nextInt(3))
        val file = "image.jpg"
        val bis = new BufferedInputStream(new FileInputStream(file))
        val bArray = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
        val bArrayString=java.util.Base64.getEncoder.encodeToString(bArray)
        var newPhoto = new Photo(albumName.albumName, bArrayString)
        var (encryptedPhotoData, encryptedKey) = newPhoto.Encrypt(publicKey)
        for {
          response <- IO(Http).ask(HttpRequest(POST, Uri(s"http://$serverAddress/user/$userName/albums/createAlbum"), entity = HttpEntity(`application/json`,albumName.makeJsonString()),headers=head)).mapTo[HttpResponse]
        } yield {
          println("UPLOAD ALBUM:"+response.entity.asString)
        }
      }
      case _ => {}

    }
    def getPhotoMap(friendList:String): Map[String,String]=
    {
      var Photos:Array[String]=friendList.split(",")
      var PhotosMap:Map[String,String] = Map()
      for( i<-0 until Photos.length )
      {
        var mapTuple:Array[String]=Photos(i).split(":")
        PhotosMap.put(mapTuple(0),mapTuple(1))
      }

      return PhotosMap
    }
    def getPostMap(friendList:String): Map[String,String]=
    {
      var Posts:Array[String]=friendList.split(",")
      var PostsMap:Map[String,String] = Map()
      for( i<-0 until Posts.length )
      {
        var mapTuple:Array[String]=Posts(i).split(":")
        PostsMap.put(mapTuple(0),mapTuple(1))
      }

      return PostsMap
    }
    def getFriendMap(friendList:String): Map[String,String]=
    {
      var friends:Array[String]=friendList.split(",")
      var friendKeyMap:Map[String,String] = Map()
      for( i<-0 until friends.length )
      {
        var mapTuple:Array[String]=friends(i).split(":")
        friendKeyMap.put(mapTuple(0),mapTuple(1))
      }

      return friendKeyMap
    }
    def chooseFriends(friendKeyMap: Map[String,String]):Map[String,String]=
    {
      var shareFriendsMap:Map[String,String]=Map()
      friendKeyMap foreach((t2 => shareFriendsMap.put(t2._1,t2._2)))

      return shareFriendsMap
    }
    def mapFriendsAndKey(Friends:Map[String,String]):Map[String,PublicKey]=
    {
      var FriendsAndKey:Map[String,PublicKey]=Map()
      Friends foreach (t2=>
      FriendsAndKey.put(t2._1,getPublicKey(t2._2)))

      return FriendsAndKey
    }
    def getEncryptedKeyMap(FriendsAndKey:Map[String,PublicKey],decryptedAesKey:String): Map[String,String] =
    {
        var rsa=new SimpleRsa
        var newFriendsMap:Map[String,String]=Map()
        FriendsAndKey foreach { t2 =>
          var encryptedkey = rsa.doSimpleRsaEncryption(decryptedAesKey, t2._2)
          newFriendsMap.put(t2._1, encryptedkey)
        }

        return newFriendsMap
    }
    def getEncryptedKeys(friendList:String,encryptedKey:String): scala.collection.immutable.Map[String,String] =
    {

      var Rsa=new SimpleRsa
      var encryptedAesKeyList:String=""
      var decryptedAesKey:String=Rsa.doRsaDecryption(encryptedKey,privateKey)
      var friendKeyMap=getFriendMap(friendList)
      var shareFriendsMap=chooseFriends(friendKeyMap)
      var FriendsAndKey=mapFriendsAndKey(shareFriendsMap)
      var FriendsEncrypted=getEncryptedKeyMap(FriendsAndKey,decryptedAesKey)
      FriendsEncrypted.put(userName,encryptedKey)
      //println("Friends Encrypted:"+FriendsEncrypted)
      val A=FriendsEncrypted.toMap
      return A
    }
    def getPublicKey(string:String): PublicKey =
    {
      //println("get public called")
      KeyFactory.getInstance("RSA").generatePublic(new  X509EncodedKeySpec(BigInt(string, 16).toByteArray))
    }
  }

  // This actor spawns the clients in the simulator.
  class UserSpawner extends Actor {

    import Globs._

    def receive = {
      case StartSpawning => {
        for (i <- 0 to no_of_clients) {
          val keyGenerator: KeyPairGenerator = KeyPairGenerator.getInstance("RSA")
          keyGenerator.initialize(2048)
          val key: KeyPair = keyGenerator.generateKeyPair()
          val UserActor = context.actorOf(Props(new User("user"+i, key)), name = "user" + i)

        }
      }
    }
  }


  //Akka Messages
  //User's Creation Messages
  case class createProfile()
  case class checkNotifications()
  case class createPost()
  case class sharePhoto()
  case class createAlbum()
  case class statusUpdate()
  case class comeToLife()
  case class scheduleActions()
  case class sharePost()
//User's read messages
  case class viewProfile()
  case class viewTimeline()
  case class viewPhoto()
  case class viewAlbum()
  case class viewFriendList()
  case class newsFeed()
  case class photoStream()
  // User's other messages
  case class sendFriendRequest()
  case class Login()
  case class callSchedulers()
  //UserSpawner's messages
  case class StartSpawning()