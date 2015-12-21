package fbserver

import java.math.BigInteger
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable.{ArrayBuffer,Map}
import scala.util.control.NonFatal
import java.io.{File, FileOutputStream}
import spray.http.{HttpData, MultipartFormData}
import Math._
import java.security._
import java.security.spec.{KeySpec, X509EncodedKeySpec}
import javax.crypto.spec.{IvParameterSpec, PBEKeySpec, SecretKeySpec}
import javax.crypto.{SecretKeyFactory, Cipher, SecretKey}

trait Backend {
import ServerStats._

//Data Structures
var userProfilesDataMap = new ConcurrentHashMap[String,EncryptedUserProfile]()
var userPostsDataMap = new ConcurrentHashMap[String,String]()
var userFriendListMap = new ConcurrentHashMap[String,ArrayBuffer[String]]()
var userTimelinesMap = new ConcurrentHashMap[String,Map[String,String]]()
var userNewsFeedsMap = new ConcurrentHashMap[String,Map[String,String]]()
var userPhotoStreamMap = new ConcurrentHashMap[String,Map[String,String]]()
var userAlbumListMap = new ConcurrentHashMap[String,ArrayBuffer[String]]()
var userAlbumDataMap = new ConcurrentHashMap[String,ArrayBuffer[String]]()
var userPhotoDataMap = new ConcurrentHashMap[String,PhotoData]()
var userPublicKeysMap = new ConcurrentHashMap[String,PublicKey]()
var userLoginMsgMap = new ConcurrentHashMap[String,String]()
var userTokenMap = new ConcurrentHashMap[String,String]()
var userNotifMap = new ConcurrentHashMap[String,ArrayBuffer[NotifData]]()

//Definitions
def registerNewUser(userData: UserDataStructure) : String = {
  try {
    if(!userPublicKeysMap.containsKey(userData.userName)) {

      userPublicKeysMap.putIfAbsent(userData.userName,userData.publicKey)
      println("Created New User: "+userData.userName)
      //println("username = " + userData.userName + "\n key = " + userData.publicKey)

      var friendList = new ArrayBuffer[String]
      var albumList = new ArrayBuffer[String]
      albumList += "DefaultAlbum"
      var photoList = new ArrayBuffer[String]
      var timeLine:Map[String,String] = Map()
      var newsFeed:Map[String,String] = Map()
      var photoStream:Map[String,String] = Map()
      var notifList = new ArrayBuffer[NotifData]

      userFriendListMap.putIfAbsent(userData.userName,friendList)
      userTimelinesMap.putIfAbsent(userData.userName,timeLine)
      userNewsFeedsMap.putIfAbsent(userData.userName,newsFeed)
      userPhotoStreamMap.putIfAbsent(userData.userName,photoStream)
      userAlbumListMap.putIfAbsent(userData.userName,albumList)
      userAlbumDataMap.putIfAbsent(userData.userName+"DefaultAlbum",photoList)
      userNotifMap.putIfAbsent(userData.userName,notifList)

      val en = formatKey(serverPubKey)
      //println("Completed: new user registratiion ")
      en
    } else {
      return "ERROR: User alrady exists!"
    }
  } catch {
      case NonFatal(t) => return "ERROR registering user!"
  }
}

def loginStep1(userID: String) : String = {
  val pubKey = userPublicKeysMap.get(userID)
  val random= new SecureRandom()
  val msg = new BigInteger(130, random).toString(16)

  userLoginMsgMap.put(userID,msg)
  val en = EncryptDecryptRSA.encrypt(msg,pubKey)
  //println(s"puzzle - $msg \n encrypted - $en")
  en
}

def loginVerify(userID: String,verifyCredentials:VerifyCredentials) : String = {
  val msg = verifyCredentials.encryptedMsg
  val decryptedMsg = EncryptDecryptRSA.decrypt(msg,serverPrivateKey)
  val storedData = userLoginMsgMap.get(userID)
  //println(s"puzzle - $decryptedMsg \n stored - $storedData")
  if(storedData == decryptedMsg) {
    val random= new SecureRandom()
    val token = new BigInteger(130, random).toString(16)
    userTokenMap.put(userID,token)
    //println(s"token - $token")
    token
  } else {
    println(s"Login Failed for user $userID")
    "Login Failed!"
  }
}

def authenticateUser(requesterID:String,enToken:String) : Boolean = {
  try {
    var token = enToken
    token = EncryptDecryptRSA.decrypt(enToken,serverPrivateKey)
    val userToken = userTokenMap.get(requesterID)
    //println(s"$requesterID -- for $userToken aga $token")
    if(userTokenMap.get(requesterID) == token) {true}
    else {false}
  } catch {
    case NonFatal(t) => return true
  }
}

def logout(requesterID:String) : String = {
  try {
    userTokenMap.remove(requesterID)
    s"$requesterID Logged out successfully!"
  } catch {
    case NonFatal(t) => "Error in logging out!"
  }
}

def updateProfile(userID: String,userProfileData: EncryptedUserProfile) : String = {
  try {
    userProfilesDataMap.putIfAbsent(userID,userProfileData)
    "Profile updated successfully"
  } catch {
    case NonFatal(t) => "Error in updating profile"
  }
}

def addFriendship(friendID: String,userID: String): String = {
  try {
    //println(s"Trying to fireng: $userID && $friendID")
    var friendLimit = 500
    if(isSameUser(userID,friendID)) {return "Same User"}

    val ulist = userFriendListMap.get(userID)
    val flist = userFriendListMap.get(friendID)

    if((flist.length > friendLimit) ||
      (ulist.length > friendLimit)) {return "Friendlimit reached!"}

    if(flist.contains(userID) || ulist.contains(friendID)) {
      return s"$userID & $friendID are already friends!"
    }

    userFriendListMap.get(userID) += friendID
    userFriendListMap.get(friendID) += userID
    totalFriendships += 1
    //println(userID + " and " + friendID + " are now friends")
    return userID + " and " + friendID + " are now friends"
  } catch {
    case NonFatal(t) => return "ERROR: Adding friend failed!"
  }
}

def sharePost(userID: String, sharePost: SharedPost): String = {
  try {
    var uuidPost = java.util.UUID.randomUUID().toString()
    userPostsDataMap.put(uuidPost,sharePost.encryptedPostContent)
    sharePost.accessList foreach {case (friend, accessKey) =>

      if (friend == userID) {
          userTimelinesMap.get(friend).put(uuidPost, accessKey)
      } else {
        //println(s"$userID - friend = $friend -- accKey = accessKey")
        userNewsFeedsMap.get(friend).put(uuidPost, accessKey)
      }
    }
    "Shared post successfully!"
  } catch {
    case NonFatal(t) => return "error"
  }
}

def serveTimeLine(userID: String): String = {
  try {
    var feedLimit = 20
    var lb = new ArrayBuffer[String]
    userTimelinesMap.get(userID) foreach {case (postID,accessKey) =>
      //println(s"$userID - postid = $postID -- accKey = $accessKey")
      lb += userPostsDataMap.get(postID) + ":" + accessKey
    }
    lb.mkString(",")
  } catch {
    case NonFatal(t) => "Error in serving timeline, try again"
  }
}

def serveUserProfile(userID: String): String = {
  try {
    userProfilesDataMap.get(userID).userData + ":" + userProfilesDataMap.get(userID).RsaEncryptedAesKey
  } catch {
    case NonFatal(t) => "Error in serving profile, try again"
  }
}

def queueProfileRequest(requesterID: String, userID: String): String = {
  try {
    val targetsKey = userPublicKeysMap.get(requesterID)
    val lockedMessage = userProfilesDataMap.get(userID).RsaEncryptedAesKey
    val nData = new NotifData("profile",userID,userID,requesterID,lockedMessage,targetsKey)
    userNotifMap.get(userID) += nData
    "Enqueued request"
  } catch {
    case NonFatal(t) => "Error in queuing profile"
  }
}

def serveFriendList(userID: String, sendKeys:Boolean): String = {
  try {
    if(sendKeys) {
      var flist = new ArrayBuffer[String]
      userFriendListMap.get(userID).foreach(friend =>
        flist += friend + ":" + formatKey(userPublicKeysMap.get(friend))
      )
      flist.mkString(",")
    } else {
      //println("--> " + userID + "--" + userFriendListMap.get(userID).mkString(","))
      userFriendListMap.get(userID).mkString(",")
    }

  } catch {
    case NonFatal(t) => "Error in serving friendlist, try again"
  }
}

def serveNewsFeed(userID: String): String = {
    try {
      var feedLimit = 20

      var lb = new ArrayBuffer[String]
      userNewsFeedsMap.get(userID) foreach {case (postID,accessKey) =>
        lb += userPostsDataMap.get(postID) + ":" + accessKey
      }
      lb.mkString(",")
    } catch {
      case NonFatal(t) => "Error in serving timeline, try again"
    }
}

def serveAlbumList(userID:String): String = {
  try {
    userAlbumListMap.get(userID).mkString(",")
  } catch {
    case NonFatal(t) => "Error in serving album list"
  }
}

def createAlbum(userID:String,albumData:AlbumData): String = {
  try {
    val alName = albumData.albumName
    userAlbumListMap.get(userID) += alName
    var photoList = new ArrayBuffer[String]
    userAlbumDataMap.putIfAbsent(userID+alName,photoList)
    s"Album $alName created successfully"
  } catch {
    case NonFatal(t) => "Error in creating album"
  }
}

def servePhotoAlbum(userID:String,albumName:String): String = {
  try {
    //println("IN VIEW PHOTO" + requesterID+albumName)
    userAlbumDataMap.get(userID+albumName).mkString(",")
  } catch {
    case NonFatal(t) => "Error in serving photo album"
  }
}

def sharePhoto(userID:String,albumName:String,photoPost:PhotoData): String = {
  try {
    var uuidIMG = java.util.UUID.randomUUID().toString()
    userPhotoDataMap.putIfAbsent(uuidIMG,photoPost)
    userPostsDataMap.put(uuidIMG,photoPost.encryptedImage)
    userAlbumDataMap.get(userID+albumName)  += uuidIMG
    photoPost.accessList foreach {case (friend, accessKey) =>
        //println(s"$userID - friend = $friend -- accKey = accessKey")
        userPhotoStreamMap.get(friend).put(uuidIMG, accessKey)
    }

    "Shared Image successfully"
  } catch {
    case NonFatal(t) => return "error"
  }
}

def servePhotoStream(userID: String): String = {
    try {
      var feedLimit = 20
      var lb = new ArrayBuffer[String]
      userPhotoStreamMap.get(userID) foreach {case (imgID,accessKey) =>
        lb += userPostsDataMap.get(imgID) + ":" + accessKey
      }
      lb.mkString(",")
    } catch {
      case NonFatal(t) => "Error in serving photostream, try again"
    }
}

def servePhoto(userID:String,requesterID:String,photoID:String): String = {
  try {
    var photoData:String = ""
    var enKey:String = ""
    userPhotoDataMap.get(photoID).accessList foreach {case (friend, accessKey) =>
      if (friend == requesterID) {
          photoData = userPhotoDataMap.get(photoID).encryptedImage + ":" + accessKey
      } else if (friend == userID) {
        enKey = accessKey
      }
    }
    if (photoData != "") {
      photoData
    } else {
      val pkey = userPublicKeysMap.get(requesterID)
      val nData = new NotifData("img",photoID,userID,requesterID,enKey,pkey)
      userNotifMap.get(userID) += nData
      "Enqueued request"
    }
  } catch {
    case NonFatal(t) => return "Error in serving photo"
  }
}

//Helper funtions
def isAllowedToAccess(requesterID:String, userID:String): Boolean = {
  isSameUser(requesterID,userID) || isFriend(requesterID,userID)
}

def isFriend(requesterID:String, userID:String): Boolean = {
  try {
    userFriendListMap.get(requesterID).contains(userID)
  } catch {
      case NonFatal(t) => false
  }
}

def isSameUser(requesterID:String, userID:String): Boolean = {
  requesterID.equalsIgnoreCase(userID)
}

def updateImageData(requesterID:String,album:String): String = {
  var img_id = "dfault"
  try {
    if(userAlbumDataMap.containsKey(requesterID+album)) {
        var album_dir = userAlbumDataMap.get(requesterID+album)
        img_id = (album_dir.length + 1).toString()
        album_dir += img_id
    } else {
        var dir = new File("media/"+requesterID+"/"+album)
        dir.mkdirs()
        var img_list = new ArrayBuffer[String]
        img_id = 1.toString()
        img_list += img_id
        userAlbumDataMap.put(requesterID+album,img_list)
    }
  } catch {
    case NonFatal(t) => "Error in updating image data"
  }
  img_id
}

def formatKey(rsaKey: java.security.PublicKey):String = {
  BigInt(rsaKey.getEncoded).toString(16)
}

} //end of trait
