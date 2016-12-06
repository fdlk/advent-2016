import scala.io.Source._

object day6 {
  val lines: List[String] = fromInputStream(getClass.getResourceAsStream("day6.txt")).getLines.toList

  lines.head.indices.map(index => lines.groupBy(_.charAt(index)).mapValues(_.size).maxBy(_._2)._1).mkString
  lines.head.indices.map(index => lines.groupBy(_.charAt(index)).mapValues(_.size).minBy(_._2)._1).mkString
}