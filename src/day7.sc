import scala.io.Source.fromInputStream
import scala.util.matching.Regex

object day7 {
  val lines: List[String] = fromInputStream(getClass.getResourceAsStream("day7.txt")).getLines.toList

  // Part 1

  def isABBA(s: String): Boolean = s.toList match {
    case a :: b :: c :: d :: Nil => a == d && b == c && a != b
  }

  def hasABBA(s: String): Boolean = s.sliding(4).exists(isABBA)

  val withinBracketsRegex: Regex = """\[([a-z]+)\]""".r

  def hasABBAWithinBrackets(line: String): Boolean =
    getBracketContents(line).exists(hasABBA)

  def getBracketContents(line: String): List[String] = withinBracketsRegex.findAllMatchIn(line).map(_.group(1)).toList

  def hasABBAOutsideBrackets(line: String): Boolean =
    hasABBA(withinBracketsRegex.replaceAllIn(line, "[]"))

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

  def supportsSSL(line: String): Boolean = {
    getABAs(withinBracketsRegex.replaceAllIn(line, "[]")).exists(
      aba => getBracketContents(line).exists(
        _.contains(bab(aba))))
  }

  lines.count(supportsSSL)
}
