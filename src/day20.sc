import scala.io.Source.fromInputStream

object day20 {
  val input: List[String] = fromInputStream(getClass.getResourceAsStream("day20.txt")).getLines.toList

  case class Blocked(from: Long, to: Long)

  def parseString(s: String): Blocked = {
    val l: List[Long] = s.split("-").map(_.toLong).toList
    Blocked(l.head, l(1))
  }

  val blocked: List[Blocked] = input.map(parseString).sortBy(_.from)

  def combine(blocked: List[Blocked]): List[Blocked] = blocked match {
    case Nil => Nil
    case b :: Nil => blocked
    case Blocked(from1, to1) :: Blocked(from2, to2) :: rest if from2 - 1 <= to1 =>
      combine(Blocked(from1, Math.max(to1, to2)) :: rest)
    case b :: rest => b :: combine(rest)
  }

  val combined: List[Blocked] = combine(blocked)
  val maxIp: Long = 4294967295L

  combined.head.to + 1
  combined.sliding(2).map({ case List(b1, b2) => b2.from - b1.to - 1 }).sum + maxIp - combined.last.to
}