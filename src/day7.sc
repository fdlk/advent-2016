import scala.io.Source.fromInputStream
import scala.util.matching.Regex

object day7 {
  val lines: List[String] = fromInputStream(getClass.getResourceAsStream("day7.txt")).getLines.toList

  // Part 1

  def isABBA(s: String): Boolean = s.toList match {
    case a :: b :: c :: d :: Nil => a == d && b == c && a != b
  }

  def hasABBA(s: String): Boolean = s.sliding(4).exists(isABBA)

  val WITHIN_BRACKETS = """\[([a-z]+)\]"""
  val withinBracketsRegex: Regex = WITHIN_BRACKETS.r

  def hasABBAWithinBrackets(line: String): Boolean = getBracketContents(line).exists(hasABBA)

  def getBracketContents(line: String): Iterator[String] = withinBracketsRegex.findAllMatchIn(line).map(_.group(1))

  def hasABBAOutsideBrackets(line: String): Boolean = line.split(WITHIN_BRACKETS).exists(hasABBA)

  def supportsTLS(line: String): Boolean = hasABBAOutsideBrackets(line) && !hasABBAWithinBrackets(line)

  lines.count(supportsTLS)

  // Part 2

  def isABA(s: String): Boolean = s.toList match {
    case a :: b :: c :: Nil => a == c && a != b
  }

  def getABAs(s: String): List[String] = s.sliding(3).filter(isABA).toList

  def bab(aba: String): String = aba.toList match {
    case a :: b :: _ :: Nil => List(b, a, b).mkString
  }

  def partsOutsideBrackets(line: String): List[String] = line.split(WITHIN_BRACKETS).toList

  def supportsSSL(line: String): Boolean = partsOutsideBrackets(line).exists(
    part => getABAs(part).exists(
      aba => getBracketContents(line).exists(
        _.contains(bab(aba))
      )
    )
  )

  lines.count(supportsSSL)
}
