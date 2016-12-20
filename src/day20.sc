import scala.io.Source.fromInputStream

object day20 {
  val (starts, ends) = fromInputStream(getClass.getResourceAsStream("day20.txt")).getLines.toList
    .map(_.split("-").toList.map(_.toLong)).unzip(l => (l(0), l(1)))
  val sortedIntervals = (starts.sorted ::: List(4294967296L), 0L :: ends.sorted).zipped

  sortedIntervals.find({ case (start, end) => start > end + 1 }).map(_._2 + 1)
  sortedIntervals.map({ case (start, end) => Math.max(0, start - end - 1) }).sum
}