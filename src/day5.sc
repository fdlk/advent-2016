import java.security.MessageDigest

import scala.annotation.tailrec

object day5 {
  val digest: MessageDigest = MessageDigest.getInstance("MD5")
  digest.update("reyedfim".getBytes)

  def initializedDigest: MessageDigest = digest.clone.asInstanceOf[MessageDigest]

  def computeHash(text: String): Array[Byte] = initializedDigest.digest(text.getBytes)

  def isInteresting(hash: Array[Byte]): Boolean = {
    (hash(0) == 0x0) && (hash(1) == 0x0) && ((hash(2) & 0xF0) == 0x0)
  }

  def format(bytes: Array[Byte]): String = bytes.map("%02x".format(_)).mkString

  val hashStream: Stream[String] = Stream.from(1).map(_.toString).map(computeHash).filter(isInteresting).map(format)

  val part1: String = hashStream.take(8).map(_.charAt(5)).toList.mkString

  def getPosAndChar(hash: String): (Char, Char) = (hash.charAt(5), hash.charAt(6))

  @tailrec
  def solve(interestingHashes: Stream[(Char, Char)], solution: List[Option[Char]]): String =
    if (solution.forall(_.isDefined)) solution.map(_.get).mkString
    else interestingHashes match {
      case (p, v) #:: rest if p > '7' => solve(rest, solution)
      case (p, v) #:: rest if solution(p.toInt - '0'.toInt).isDefined => solve(rest, solution)
      case (p, v) #:: rest =>
        println(solution.map(_.getOrElse('_')).mkString)
        solve(rest, solution.updated(p.toInt - '0', Some(v)))
    }

  val part2: String = solve(hashStream.map(getPosAndChar), List(None, None, None, None, None, None, None, None))
}