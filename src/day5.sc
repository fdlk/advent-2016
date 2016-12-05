import java.security.MessageDigest

object day5 {
  val digest: MessageDigest = MessageDigest.getInstance("MD5")
  digest.update("reyedfim".getBytes)

  def initializedDigest = digest.clone.asInstanceOf[MessageDigest]

  def computeHash(text: String): Array[Byte] = initializedDigest.digest(text.getBytes)

  def check(hash: Array[Byte]): Boolean = {
    (hash(0) == 0x0) && (hash(1) == 0x0) && ((hash(2) | 0xF) == 0xF)
  }

  def log(hash: String): (Char, Char) = {
    val result = (hash.charAt(5), hash.charAt(6))
    println(result)
    result
  }

  def format(bytes: Array[Byte]): String = bytes.map("%02x".format(_)).mkString

  val hashStream = Stream.from(1).map(_.toString).map(computeHash).filter(check).map(format).map(log)
  hashStream.take(100).toList

  //  def hash2(text: String): Boolean = initializedDigest.digest(text.getBytes).take(3).sameElements(Array(0, 0, 0))
  //
  //  Stream.from(hash1.get.toInt) map (_.toString) find hash2
}