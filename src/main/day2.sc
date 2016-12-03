import scala.io.Source._

object day2 {
  val lines: List[String] = fromInputStream(getClass.getResourceAsStream("day2.txt")).getLines.toList

  type Board = List[List[Option[Char]]]

  val board1: Board = List("123", "456", "789").map(_.toList.map(c => Some(c)))
  val board2: Board = List("  1  ", " 234 ", "56789", " ABC ", "  D  ").map(_.toList.map(c => if (c == ' ') None else Some(c)))

  case class State(row: Int, col: Int, board: Board) {
    def mapLine(line: String): State = {
      line.toList.foldLeft(this)((state, char) => state.mapCharIfCodeExists(char))
    }

    def mapChar(char: Char): State = char match {
      case 'U' if row > 0 => State(row - 1, col, board)
      case 'D' if row < board.size - 1 => State(row + 1, col, board)
      case 'L' if col > 0 => State(row, col - 1, board)
      case 'R' if col < board.size - 1 => State(row, col + 1, board)
      case default => this
    }

    def mapCharIfCodeExists(char: Char): State = {
      val mapped = mapChar(char)
      if (mapped.code().isDefined) mapped else this
    }

    def code(): Option[Char] = board(row)(col)
  }

  val code1: String = lines
    .scanLeft(State(1, 1, board1))((state, line) => state.mapLine(line))
    .tail
    .map(_.code().get)
    .mkString

  val code2: String = lines
    .scanLeft(State(2, 0, board2))((state, line) => state.mapLine(line))
    .tail
    .map(_.code().get)
    .mkString
}