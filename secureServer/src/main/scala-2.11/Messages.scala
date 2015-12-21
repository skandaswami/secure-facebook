package fbserver

import spray.json
import spray.json.DefaultJsonProtocol
import spray.httpx.SprayJsonSupport
import java.security._
import scala.collection.mutable.{ArrayBuffer}
import java.security.spec.{KeySpec, X509EncodedKeySpec}
import javax.crypto.spec.{IvParameterSpec, PBEKeySpec, SecretKeySpec}
import javax.crypto.{SecretKeyFactory, Cipher, SecretKey}
import spray.json._

object ServerStats {
  var httpRequestsProcessed:Long = 0.toLong
  var prevRequests:Long = 0.toLong
  var upTime:Integer = 0.toInt
  var totalPhotos:Long = 0.toLong
  var totalPosts:Long = 0.toLong
  var totalUsers:Long = 0.toLong
  var totalFriendships:Long = 0.toLong
  var totalNewsFeedRequests:Long = 0.toLong

  val keyGenerator: KeyPairGenerator = KeyPairGenerator.getInstance("RSA")
  keyGenerator.initialize(2048)
  val key: KeyPair = keyGenerator.generateKeyPair()
  val serverPrivateKey = key.getPrivate()
  val serverPubKey = key.getPublic()
}

object EncryptDecryptRSA {

  def encrypt(aes_key_params:String,publicKey: PublicKey):String = {
    //println("\nDoing RsaEncryption!\n")
    var rsaCipher:Cipher = Cipher.getInstance("RSA")
    rsaCipher.init(Cipher.ENCRYPT_MODE, publicKey)
    var encryptedAesKey = rsaCipher.doFinal(aes_key_params.getBytes)
    var encryptString:String = java.util.Base64.getEncoder.encodeToString(encryptedAesKey)
    //println("encrypted string: "+encryptString)
    encryptString
  }

  def decrypt(encryptedAesKey:String,privateKey: PrivateKey): String = {
    //println("\nDoing RsaDecryption!\n")
    var encryptedBytes = java.util.Base64.getDecoder.decode(encryptedAesKey)
    var cipher:Cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.DECRYPT_MODE, privateKey)
    var decryptedAesKey = cipher.doFinal(encryptedBytes)
    var decryptedKeyString= new String(decryptedAesKey)
    //println("Decrypted key string:"+decryptedKeyString)
    decryptedKeyString
  }
}

object JsonSupport extends DefaultJsonProtocol with SprayJsonSupport {

  implicit object OverrideBigIntJsonFormat extends RootJsonFormat[BigInt] {
    def write(i: BigInt) = JsString(i.toString(16))

    def read(i: JsValue) = i match {
      case JsString(num) => BigInt(num, 16)
      case _ => deserializationError("Error in reading big int")
    }
  }
  implicit object PublicKeyJsonFormat extends RootJsonFormat[PublicKey] {
    def write(key: PublicKey) = JsString(BigInt(key.getEncoded).toString(16))

    def read(value: JsValue) = value match {
      case JsString(rsaKey) => KeyFactory.getInstance("RSA").generatePublic(new X509EncodedKeySpec(BigInt(rsaKey, 16).toByteArray))
      case _ => deserializationError("Error in reading public key")
    }
  }

  implicit val UserDataStructureFormat = jsonFormat2(UserDataStructure)
  implicit val EncryptedUserProfileFormat = jsonFormat2(EncryptedUserProfile)
  implicit val AlbumDataFormat = jsonFormat1(AlbumData)
  implicit val PhotoDataFormat = jsonFormat2(PhotoData)
  implicit val VerifyCredentialsFormat = jsonFormat1(VerifyCredentials)
  implicit val SharedPostFormat = jsonFormat2(SharedPost)
  implicit val NotifDataFormat = jsonFormat6(NotifData)
}

//Classes
case object OutputStats
case class UserDataStructure(userName:String, publicKey:PublicKey)
case class AlbumData(albumName:String)
case class PhotoData(encryptedImage:String,accessList:Map[String,String])
case class VerifyCredentials(encryptedMsg: String)
case class EncryptedUserProfile(userData:String,RsaEncryptedAesKey:String)
case class SharedPost(encryptedPostContent:String,accessList:Map[String,String])
case class NotifData(contentType:String,contentID:String,userID:String,
  friendID:String,encryptedLock:String,friendKey:PublicKey)
