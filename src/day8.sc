import scala.io.Source.fromInputStream
import scala.util.matching.Regex

object day8 {

  val lines: List[String] = fromInputStream(getClass.getResourceAsStream("day8.txt")).getLines.toList

  case class Board(leds: List[List[Boolean]]) {
    override def toString: String = "\n" + leds.map(_.map(if (_) '#' else '.').mkString).mkString("\n")

    def isLit(row: Int, col: Int): Boolean = leds(row)(col)

    def rows: Int = leds.size

    def cols: Int = leds.head.size

    def newBoard(isLit: (Int, Int) => Boolean): Board = Board.newBoard(rows, cols, isLit)

    def rect(numCols: Int, numRows: Int): Board = newBoard(
      (row, col) => if (row < numRows && col < numCols) true else isLit(row, col)
    )

    def rotateCol(rotCol: Int, by: Int): Board = newBoard(
      (row, col) => if (col == rotCol) isLit((row + rows - by % rows) % rows, col) else isLit(row, col)
    )

    def rotateRow(rotRow: Int, by: Int): Board = newBoard(
      (row, col) => if (row == rotRow) isLit(row, (col + cols - by % cols) % cols) else isLit(row, col)
    )

    def voltage: Int = leds.flatten.count(led => led)
  }

  object Board {
    def newBoard(rows: Int, cols: Int, isLit: (Int, Int) => Boolean): Board =
      Board(List.range(0, rows).map(row => List.range(0, cols).map(col => isLit(row, col))))

    def emptyBoard(rows: Int, cols: Int): Board = newBoard(rows, cols, (_, _) => false)
  }

  val rect: Regex = """rect ([0-9]+)x([0-9]+)""".r
  val rotateRow: Regex = """rotate row y=([0-9]+) by ([0-9]+)""".r
  val rotateCol: Regex = """rotate column x=([0-9]+) by ([0-9]+)""".r

  def update(board: Board, line: String): Board = line match {
    case rect(rows, cols) => board.rect(rows.toInt, cols.toInt)
    case rotateRow(row, by) => board.rotateRow(row.toInt, by.toInt)
    case rotateCol(col, by) => board.rotateCol(col.toInt, by.toInt)
  }

  val emptyBoard: Board = Board.emptyBoard(6, 50)
  val doneBoard: Board = lines.foldLeft(emptyBoard)(update)

  doneBoard.voltage
}