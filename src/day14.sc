import java.security.MessageDigest

object day14 {
  val digest: MessageDigest = MessageDigest.getInstance("MD5")
  //  digest.update("jlmsuwbz".getBytes)
  digest.update("jlmsuwbz".getBytes)

  def initializedDigest: MessageDigest = digest.clone.asInstanceOf[MessageDigest]

  def computeHash(text: String): Array[Byte] = initializedDigest.digest(text.getBytes)

  val hexCode = "0123456789abcdef".toCharArray

  def format(bytes: Array[Byte]): String = {

    val r: StringBuilder = new StringBuilder(bytes.length * 2)
    for (b <- bytes) {
      r.append(hexCode((b >> 4) & 0xF))
      r.append(hexCode((b & 0xF)))
    }
    return r.toString
  }

  def firstTriple(hash: String): Option[Char] = hash.toSeq.sliding(3).map(_.toSet).find(_.size == 1).map(_.head)

  def containsFive(hash: String, c: Char) = hash.contains(Range(0, 5).map(_ => c).mkString)

  def headIsKey(s: Stream[String]): Boolean =
    firstTriple(s.head).map(c => s.tail.take(1000).exists(containsFive(_, c))).getOrElse(false)

  def findKey(s: Stream[String], index: Int, keysLeft: Int): Int = {
    val isKey: Boolean = headIsKey(s)
    if (isKey && keysLeft == 1) index
    else findKey(s.tail, index + 1, if (isKey) keysLeft - 1 else keysLeft)
  }

  findKey(Stream.from(1).map(_.toString).map(computeHash).map(format), 1, 64)
}