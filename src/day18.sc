object day18 {
  val input: String = ".^^..^...^..^^.^^^.^^^.^^^^^^.^.^^^^.^^.^^^^^^.^...^......^...^^^..^^^.....^^^^^^^^^....^^...^^^^..^"

  def nextChar(prev: String): Char = if (List("^^.", ".^^", "^..", "..^").contains(prev)) '^' else '.'

  def next(line: String): String = ("." + line + ".").sliding(3).map(nextChar).mkString

  def safeTiles(line: String): Int = line.count(_ == '.')

  Stream.iterate(input, 40)(next).map(safeTiles).sum
  Stream.iterate(input, 400000)(next).map(safeTiles).sum
}