import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter.printHexBinary

object day14 {
  val key = "jlmsuwbz"

  def md5(s: String): String = printHexBinary(MessageDigest.getInstance("MD5").digest(s.getBytes)).toLowerCase

  def stretchedHash(hash: String): String = Iterator.iterate(hash)(md5).drop(2017).next

  def firstTriple(hash: String): Option[Char] = hash.toSeq.sliding(3).map(_.toSet).find(_.size == 1).map(_.head)

  def containsFive(hashes: Stream[String], c: Char): Boolean = hashes.exists(_.contains(List.fill(5)(c).mkString))

  def keyIndices(s: Stream[(Int, String)]): Stream[Int] = s match {
    case (index, hash) #:: rest => firstTriple(hash) match {
      case Some(c) if containsFive(rest.map(_._2).take(1000), c) => index #:: keyIndices(rest)
      case default => keyIndices(rest)
    }
  }

  keyIndices(Stream.from(0).map(index => (index, md5(key + index)))).drop(63).head
  keyIndices(Stream.from(0).map(index => (index, stretchedHash(key + index)))).drop(63).head
}