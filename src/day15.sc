import scala.io.Source.fromInputStream
import scala.util.matching.Regex

object day15 {
  val part1Instructions: List[String] = fromInputStream(getClass.getResourceAsStream("day15.txt")).getLines.toList

  val instructionRegex: Regex = """Disc #([0-9]+) has ([0-9]+) positions; at time=0, it is at position ([0-9]+).""".r

  def isSolutionTime(time: Int, instructions: List[String]): Boolean = instructions.forall {
    case instructionRegex(nr, positions, initial) => (initial.toInt + time + nr.toInt) % positions.toInt == 0
  }

  Stream.from(0).find(isSolutionTime(_, part1Instructions)).get
  Stream.from(0).find(isSolutionTime(_, "Disc #7 has 11 positions; at time=0, it is at position 0." :: part1Instructions)).get
}