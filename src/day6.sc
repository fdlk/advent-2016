import scala.io.Source._

object day6 {
  val lines: List[String] = fromInputStream(getClass.getResourceAsStream("day6.txt")).getLines.toList

  lines.head.indices.map(index => lines.groupBy(_.charAt(index)).maxBy(_._2.size)._1).mkString
  lines.head.indices.map(index => lines.groupBy(_.charAt(index)).minBy(_._2.size)._1).mkString
}