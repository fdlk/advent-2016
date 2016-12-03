import scala.io.Source._

object day3 {
  val lines: List[String] = fromInputStream(getClass.getResourceAsStream("day3.txt")).getLines.toList

  type Triangle = List[Int]
  val triangles: List[Triangle] = lines.map(_.trim.split("\\s+").toList.map(_.toInt))

  def isPossible(triangle: Triangle): Boolean =
    (for {
      dims <- triangle.permutations
      dim1 :: dim2 :: dim3 :: _ = dims
      if dim1 + dim2 <= dim3
    } yield false).isEmpty

  triangles.count(isPossible)

  triangles.grouped(3).map(
    group => group.indices.map(
      index => group.map(triangle => triangle(index))
    )
  ).flatten.count(isPossible)
}