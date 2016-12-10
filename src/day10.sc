import scala.io.Source.fromInputStream
import scala.util.matching.Regex

object day10 {
  type StateMap = Map[String, Set[Int]]

  val instructions: List[String] =
    fromInputStream(getClass.getResourceAsStream("day10.txt")).getLines.toList

  val grab: Regex =
    """value ([0-9]+) goes to (bot [0-9]+)""".r
  val give: Regex = """(bot [0-9]+) gives low to ((bot|output) [0-9]+) and high to ((bot|output) [0-9]+)""".r

  def followInstruction(state: StateMap, i: String): StateMap = i match {
    case give(from, low, _, high, _) if state(from).size == 2 =>
      state
        .updated(from, Set())
        .updated(low, state(low) + state(from).min)
        .updated(high, state(high) + state(from).max)
    case grab(chip, bot) =>
      state.updated(bot, state(bot) + chip.toInt)
    case default => state
  }

  // Define solution
  def solution(stateMap: StateMap): Option[(String, Int)] = for (
    part1 <- stateMap.find(target => target._2 == Set(61, 17)).map(_._1);
    nrs = Set("output 0", "output 1", "output 2").flatMap(stateMap(_));
    part2 = nrs.product if nrs.size == 3
  ) yield (part1, part2)

  // And solve!
  def solve(state: StateMap): (String, Int) = {
    // scanLeft cause it keeps the intermediate states and the part 1 bot may have handed off the chips
    // by the time we get to the end of the instructions
    val states: Seq[StateMap] = instructions.scanLeft(state)(followInstruction)
    states.flatMap(solution).headOption.getOrElse(solve(states.last))
  }

  val initialState: StateMap = Map().withDefaultValue(Set())
  solve(initialState)
}