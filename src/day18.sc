object day18 {
  val input: String = """.^^..^...^..^^.^^^.^^^.^^^^^^.^.^^^^.^^.^^^^^^.^...^......^...^^^..^^^.....^^^^^^^^^....^^...^^^^..^"""

  def nextChar(prev: String): Char =
    if (List("^^.", ".^^", "^..", "..^").contains(prev)) '^' else '.'

  def next(line: String): String = ("." + line + ".").sliding(3).map(nextChar).mkString

  def safeTiles(line: String, found: Int, gensLeft: Int): Int =
    if (gensLeft == 0)
      found + line.count(_ == '.')
    else
      safeTiles(next(line), found + line.count(_ == '.'), gensLeft - 1)

  safeTiles(input, 0, 40 - 1)
  safeTiles(input, 0, 400000 - 1)
}