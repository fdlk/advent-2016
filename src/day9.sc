import scala.io.Source._
import scala.util.matching.Regex

object day9 {
  val input: String =
    fromInputStream(getClass.getResourceAsStream("day9.txt")).getLines.toList.head

  val instructionRegex: Regex = """^(\(([0-9]+)x([0-9]+)\)).*""".r

  def count(startIndex: Int, charsLeft: Int, soFar: Long, recurse: Boolean): Long = {
    if (charsLeft <= 0) soFar
    else {
      input.substring(startIndex) match {
        case instructionRegex(w, c, t) =>
          val times: Long = t.toLong
          val charCount: Int = c.toInt
          val whole: Int = w.length
          times * (if (recurse) count(startIndex + whole, charCount, 0, recurse) else charCount) +
            count(startIndex + whole + charCount, charsLeft - whole - charCount, soFar, recurse)
        case default => count(startIndex + 1, charsLeft - 1, soFar + 1, recurse)
      }
    }
  }

  count(0, input.length, 0, recurse = false)
  count(0, input.length, 0, recurse = true)
}