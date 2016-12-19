import scala.annotation.tailrec

object day19 {
  val nElves: Int = 3014603

  @tailrec
  def part1(n: Int, jn: Int): Int =
    if (n == nElves)
      jn
    else
      part1(n + 1, (jn + 1) % (n + 1) + 1)

  part1(1, 1)

  @tailrec
  def part2(n: Int, jn: Int): Int =
    if (n == nElves)
      jn
    else
      part2(n + 1,
        if (jn == n) 1
        else if (jn < (n + 1) / 2)
          jn + 1
        else
          jn + 2)

  part2(1, 1)
}