import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter.printHexBinary

object day17 {
  val passcode = "yjjvjgan"

  def md5(s: String): String = printHexBinary(MessageDigest.getInstance("MD5").digest(s.getBytes)).toLowerCase

  case class State(movesTaken: String, row: Int, col: Int) {
    val hash: String = md5(passcode + movesTaken).substring(0, 4)

    def isOpen(index: Int): Boolean = "bcdef".contains(hash.charAt(index))

    def up: Option[State] = if (isOpen(0) && row > 0) Some(State(movesTaken + 'U', row - 1, col)) else None

    def down: Option[State] = if (isOpen(1) && row < 3) Some(State(movesTaken + 'D', row + 1, col)) else None

    def left: Option[State] = if (isOpen(2) && col > 0) Some(State(movesTaken + 'L', row, col - 1)) else None

    def right: Option[State] = if (isOpen(3) && col < 3) Some(State(movesTaken + 'R', row, col + 1)) else None

    def neighbors: List[State] = for (direction <- List(up, down, left, right) if direction.isDefined) yield direction.get

    def finished: Boolean = row == 3 && col == 3
  }

  def nextGen(states: List[State]): List[State] = for (
    state <- states;
    nextState <- state.neighbors
  ) yield nextState

  val gen0: List[State] = List(State("", 0, 0))

  def part1(gen: List[State]): String = {
    gen.find(_.finished).map(_.movesTaken).getOrElse(part1(nextGen(gen)))
  }

  part1(gen0)

  def part2(gen: List[State], longestSoFar: Int): Int = {
    if (gen.isEmpty) longestSoFar
    else {
      val (done, notDone) = nextGen(gen).partition(_.finished)
      val newLongest: Int = done.headOption.map(_.movesTaken.length).getOrElse(longestSoFar)
      part2(notDone, newLongest)
    }
  }

  part2(gen0, 0)
}