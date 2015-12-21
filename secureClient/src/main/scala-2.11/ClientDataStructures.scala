
/*
This contains the Datastructure definitions for the client side, with function definitions to convert the datastructures
into JSON strings which are sent through HTTP Post requests.
*/

package fbclient

import java.io.{ObjectOutputStream, FileOutputStream}
import java.math.BigInteger
import java.security._
import java.security.spec.{KeySpec, X509EncodedKeySpec}
import javax.crypto.spec.{IvParameterSpec, PBEKeySpec, SecretKeySpec}
import javax.crypto.{SecretKeyFactory, Cipher, SecretKey}
import org.apache.commons.codec.binary.Hex
import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol
import spray.json._

//Json Conversion Formats for DataStructures.
object JsonSupport extends DefaultJsonProtocol with SprayJsonSupport {

  implicit object MyBigIntJsonFormat extends RootJsonFormat[BigInt] {
    def write(i: BigInt) = JsString(i.toString(16))

    def read(i: JsValue) = i match {
      case JsString(num) => BigInt(num, 16)
      case _ => deserializationError("bigint string expected")
    }
  }
  implicit object PublicKeyJsonFormat extends RootJsonFormat[PublicKey] {
    def write(k: PublicKey) = JsString(BigInt(k.getEncoded).toString(16))

    def read(t: JsValue) = t match {
      case JsString(key) => KeyFactory.getInstance("RSA").generatePublic(new X509EncodedKeySpec(BigInt(key, 16).toByteArray))
      case _ => deserializationError("Deserialization error Public key to Byte Array.")
    }
  }
  implicit val UserFormat = jsonFormat2(UserDataStructure)
  implicit val ProfileFormat=jsonFormat4(Profile)
  implicit val EncryptedProfileFormat=jsonFormat2(EncryptedUserProfile)
  implicit val PostFormat=jsonFormat1(Post)
  implicit val encryptedPostFormat=jsonFormat2(EncryptedPost)
  implicit val PhotoFormat=jsonFormat2(Photo)
  implicit val encryptedPhotoFormat=jsonFormat2(EncryptedPhoto)
  implicit val ServerPublicKeyFormat=jsonFormat1(ServerPublicKey)
  implicit val EncryptedStringFormat=jsonFormat1(EncryptedString)
  implicit val sharedPostFormat=jsonFormat2(sharedPost)
  implicit val sharedPhotoFormat=jsonFormat2(sharedPhoto)
  implicit val albumNameFormat=jsonFormat1(AlbumName)


}

//ENCRYPTION AND DECRYPTION UTILITY FOR RSA, AES HYBRID
class Encryption(password:String)
{
  var initVector:Array[Byte] = null
  var Salt:Array[Byte] = null

  def doAesEncryption(data:String):(String,String) =
  {
    var factory:SecretKeyFactory  = null
    var tmp:SecretKey  =null
    var rnd :SecureRandom = new SecureRandom
    Salt=new Array[Byte](16)
    rnd.nextBytes(Salt)
    factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
    var spec:KeySpec  = new PBEKeySpec (password.toCharArray , Salt, 65536, 128)
    tmp = factory.generateSecret (spec)
    var secret:SecretKey = new SecretKeySpec (tmp.getEncoded, "AES")
    var encryptCipher = Cipher.getInstance ("AES/CBC/PKCS5Padding")
    encryptCipher.init (Cipher.ENCRYPT_MODE, secret)
    var params:AlgorithmParameters = encryptCipher.getParameters
    initVector = params.getParameterSpec(classOf[IvParameterSpec]).getIV
    var aesKeyParameters:String=password+"\n"+Hex.encodeHexString(Salt)+"\n"+Hex.encodeHexString((initVector))
    var encrypt = encryptCipher.doFinal(data.getBytes)
    var encryptString:String=java.util.Base64.getEncoder.encodeToString(encrypt)
    return (encryptString,aesKeyParameters)
  }

  def doRsaEncryption(aes_key_params:String,publicKey: PublicKey):String=
  {
    var rsaCipher:Cipher = Cipher.getInstance("RSA")
    rsaCipher.init(Cipher.ENCRYPT_MODE, publicKey)
    var encryptedAesKey = rsaCipher.doFinal(aes_key_params.getBytes)
    var encryptString:String=java.util.Base64.getEncoder.encodeToString(encryptedAesKey)
    return encryptString
  }

  def getSalt:Array[Byte] =
  {
    return Salt
  }

  def getInitVector:Array[Byte]=
  {
    return initVector
  }
}
class Decryption
{
  def doRsaDecryption(encryptedAesKey:String,privateKey: PrivateKey): Array[String] =
  {
    //println("\nDoing RsaDecryption!\n")
    var encryptedBytes=java.util.Base64.getDecoder.decode(encryptedAesKey)
    var cipher:Cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.DECRYPT_MODE, privateKey)
    var decryptedAesKey = cipher.doFinal(encryptedBytes)
    var decryptedKeyString=new String(decryptedAesKey)
    //println("Decrypted key string:"+decryptedKeyString)
    var splitKey:Array[String]=decryptedKeyString.split("\\n")
    return splitKey
  }

  def doAesDecryption (initvec:String,salt:String,password:String,encryptedData:String):String=
  {

    var factory:SecretKeyFactory  = null
    var tmp:SecretKey  = null
    var secret:SecretKey = null

    var encryptedBytes=java.util.Base64.getDecoder.decode(encryptedData)
    var Salt = Hex.decodeHex (salt.toCharArray)
    var initVector = Hex.decodeHex (initvec.toCharArray)
    factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
    var spec:KeySpec = new PBEKeySpec(password.toCharArray, Salt, 65536, 128)
    tmp = factory.generateSecret(spec)
    secret = new SecretKeySpec(tmp.getEncoded, "AES")
    var decryptCipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    decryptCipher.init(Cipher.DECRYPT_MODE, secret, new IvParameterSpec(initVector))
    var decryptedAesData=decryptCipher.doFinal(encryptedBytes)
    var decryptedString : String = new String(decryptedAesData)
    return decryptedString
  }
}

//CLASS THAT PERFORMS ONLY RSA ENCRYPTION AND DECRYPTION
class SimpleRsa
{
  def doSimpleRsaEncryption(unEncrypted:String,RsaPublicKey:PublicKey): String =
  {
    var rsaCipher:Cipher = Cipher.getInstance("RSA")
    rsaCipher.init(Cipher.ENCRYPT_MODE,RsaPublicKey)
    var encryptedData=rsaCipher.doFinal(unEncrypted.getBytes())
    var encryptString:String=java.util.Base64.getEncoder.encodeToString(encryptedData)
    return encryptString
  }
  def doRsaDecryption(encryptedData:String,privateKey: PrivateKey): String =
  {
    var encryptedBytes=java.util.Base64.getDecoder.decode(encryptedData)
    var cipher:Cipher = Cipher.getInstance("RSA")
    cipher.init(Cipher.DECRYPT_MODE, privateKey)
    var decryptedAesKey = cipher.doFinal(encryptedBytes)
    var decryptedString=new String(decryptedAesKey)
    return decryptedString
  }
}
//DEFINITION OF REQUIRED DATA STRUCTURES

//POSTS THAT ARE UPDATED IN THE USER'S TIMELINE AND HIS NEWSFEED
case class sharedPost(encryptedPostContent:String,accessList:scala.collection.immutable.Map[String,String])
{
  import JsonSupport._
  def makeJsonString(): String =
  {
    this.toJson.toString()
  }
}
// PHOTOS THAT ARE SHARED IN A USER'S PHOTOSTREAM
case class sharedPhoto(encryptedImage:String,accessList:Map[String,String])
{
  import JsonSupport._
  def makeJsonString(): String =
  {
    this.toJson.toString()
  }
}
//PUBLIC KEY
case class ServerPublicKey(publicKey: PublicKey)
{
  import JsonSupport._
  def makeJsonString(): String =
  {
    this.toJson.toString()
  }
}
//ENCRYPTED STRINGS
case class EncryptedString(encryptedMsg:String)
{
  import JsonSupport._
  def makeJsonString(): String =
  {
    this.toJson.toString()
  }
}
//USER
case class UserDataStructure(userName:String,publicKey:PublicKey)
{
  import JsonSupport._
  def makeJsonString():String=
  {
    return this.toJson.toString()
  }
}

//USER PROFILE
case class Profile(var name:String,var age:String,var sex:String,var relationShipStatus:String)
{
  import JsonSupport._
  def makeJsonString():String=
  {
    return this.toJson.toString()
  }
  def Encrypt(publicKey: PublicKey): (String,String) =
  {
    var random= new SecureRandom()
    var password = new BigInteger(130, random).toString(32)
    var encryptor=new Encryption(password)
    var unencryptedProfileData=name+"\n"+age+"\n"+sex+"\n"+relationShipStatus+"\n"
    var (encryptedProfileData,aesKeyParameters)=encryptor.doAesEncryption(unencryptedProfileData)
    var RSAencryptedAesKey=encryptor.doRsaEncryption(aesKeyParameters,publicKey)
    return (encryptedProfileData,RSAencryptedAesKey)
  }
  def print(): Unit =
  {
    println("\nUSER PROFILE DATA\n")
    println("\n Name:"+name+" Age:"+age+" Sex:"+sex+" Relationship Status:"+relationShipStatus+"\n")
  }

}

//Posts
case class Post(var postContent:String)
{
  import JsonSupport._
  def makeJsonString():String=
  {
    return this.toJson.toString()
  }
  def Encrypt(publicKey: PublicKey): (String,String) =
  {
    var random= new SecureRandom()
    var password = new BigInteger(130, random).toString(32)
    var encryptor=new Encryption(password)
    var unencryptedPostData=postContent
    var (encryptedPostData,aesKeyParameters)=encryptor.doAesEncryption(unencryptedPostData)
    var RSAencryptedAesKey=encryptor.doRsaEncryption(aesKeyParameters,publicKey)
    return (encryptedPostData,RSAencryptedAesKey)
  }
  def print(): Unit =
  {
    println("\n Post Content:"+postContent)
  }
}
//PHOTO
case class Photo(var albumName:String,var imageData:String)
{
  import JsonSupport._
  def makeJsonString():String=
  {
    return this.toJson.toString()
  }
  def Encrypt(publicKey: PublicKey): (String,String) =
  {
    var random= new SecureRandom()
    var password = new BigInteger(130, random).toString(32)
    var encryptor=new Encryption(password)
    var unencryptedPhotoData=albumName+"\n"+imageData
    var (encryptedPhotoData,aesKeyParameters)=encryptor.doAesEncryption(unencryptedPhotoData)
    var RSAencryptedAesKey=encryptor.doRsaEncryption(aesKeyParameters,publicKey)
    return (encryptedPhotoData,RSAencryptedAesKey)
  }
  def print(): Unit =
  {
    var ImageString:String=new String(imageData)
    println("\n"+"Album Name:"+albumName+" Photo Content:"+ImageString)
  }
}
//ENCRYPTED POST
case class EncryptedPost(var encryptedPostContent:String,var RsaEncryptedAesKey:String)
{
  import JsonSupport._
  def makeJsonString():String=
  {
    return this.toJson.toString()
  }
  def Decrypt(privateKey: PrivateKey): Post =
  {
    var decryptor=new Decryption
    var aesKey=decryptor.doRsaDecryption(RsaEncryptedAesKey,privateKey)
    var(password,salt,initVector)=(aesKey(0),aesKey(1),aesKey(2))
    var decryptedPostContent=decryptor.doAesDecryption(initVector,salt,password,encryptedPostContent)
    var decryptedFields=decryptedPostContent.split("\\n")
    var userPost:Post=new Post(decryptedFields(0))
    return userPost
  }
}
// ENCRYPTED USER PROFILE STRUCTURE
case class EncryptedUserProfile(userData:String,RsaEncryptedAesKey:String)
{
  import JsonSupport._
  def makeJsonString():String=
  {
    return this.toJson.toString()
  }
  def Decrypt(privateKey: PrivateKey): Profile =
  {
    var decryptor=new Decryption
    var aesKey=decryptor.doRsaDecryption(RsaEncryptedAesKey,privateKey)
    var(password,salt,initVector)=(aesKey(0),aesKey(1),aesKey(2))
    var decryptedUserData=decryptor.doAesDecryption(initVector,salt,password,userData)
    var decryptedFields=decryptedUserData.split("\\n")
    var userProfile:Profile=new Profile(decryptedFields(0),decryptedFields(1),decryptedFields(2),decryptedFields(3))
    return userProfile
  }
}
//ENCRYPTED PHOTO
case class EncryptedPhoto(var encryptedImageData:String,RsaEncryptedAesKey:String)
{
  import JsonSupport._
  def makeJsonString():String=
  {
    return this.toJson.toString()
  }
  def Decrypt(privateKey: PrivateKey): Photo =
  {
    var decryptor=new Decryption
    var aesKey=decryptor.doRsaDecryption(RsaEncryptedAesKey,privateKey)
    var(password,salt,initVector)=(aesKey(0),aesKey(1),aesKey(2))
    var decryptedPhotoContent=decryptor.doAesDecryption(initVector,salt,password,encryptedImageData)
    var decryptedFields=decryptedPhotoContent.split("\\n")
    var userPhoto:Photo=new Photo(decryptedFields(0),decryptedFields(1))
    return userPhoto
  }
}
//CREATING A NEW ALBUM NAME
case class AlbumName(albumName:String)
{
  import JsonSupport._
  def makeJsonString(): String =
  {
    return this.toJson.toString()
  }
}


