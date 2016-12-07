import scala.io.Source.fromInputStream
import scala.util.matching.Regex

object day7 {
  val lines: List[String] = fromInputStream(getClass.getResourceAsStream("day7.txt")).getLines.toList

  // Part 1

  val ABBARegex: Regex = """([a-z])([a-z])\2\1""".r

  def isABBA(s: String): Boolean = ABBARegex.findFirstIn(s).map(_.toList).exists(_ match { case (a :: b :: _) => a != b })

  def hasABBA(s: String): Boolean =
    s.sliding(4).exists(isABBA)

  val withinBracketsRegex: Regex = """\[([a-z]+)\]""".r

  def hasABBAWithinBrackets(line: String): Boolean =
    getBracketContents(line).exists(hasABBA)

  def getBracketContents(line: String): List[String] = withinBracketsRegex.findAllMatchIn(line).map(_.group(1)).toList

  def hasABBAOutsideBrackets(line: String): Boolean =
    hasABBA(withinBracketsRegex.replaceAllIn(line, "X"))

  def supportsTLS(line: String): Boolean = hasABBAOutsideBrackets(line) && !hasABBAWithinBrackets(line)

  lines.count(supportsTLS)

  // Part 2

  val ABARegex: Regex = """([a-z])([a-z])\1""".r

  def isABA(s: String): Boolean =
    ABARegex.findFirstIn(s).exists(m => m.charAt(0) != m.charAt(1))

  def getABAs(s: String): List[String] = s.sliding(3).filter(isABA).toList

  def bab(aba: String): String = aba.toList match {
    case a :: b :: _ :: Nil => List(b, a, b).mkString
  }

  def supportsSSL(line: String): Boolean = {
    getABAs(withinBracketsRegex.replaceAllIn(line, "X"))
      .exists(aba => getBracketContents(line)
        .exists(_.contains(bab(aba))))
  }

  val examples = List("aba[bab]xyz", "xyx[xyx]xyx", "aaa[kek]eke")

  lines.count(supportsSSL)
}
