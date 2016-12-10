import scala.io.Source.fromInputStream
import scala.util.matching.Regex

object day10 {
  val input: List[String] =
    fromInputStream(getClass.getResourceAsStream("day10.txt")).getLines.toList

  // The bot world
  sealed trait Instruction

  sealed trait Target

  case class Bot(nr: Int) extends Target

  case class Output(nr: Int) extends Target

  case class Give(from: Bot, low: Target, high: Target) extends Instruction

  case class Grab(bot: Bot, value: Int) extends Instruction

  type StateMap = Map[Target, Set[Int]]

  def followInstruction(state: StateMap, i: Instruction): StateMap = i match {
    case Give(from, low, high) if state(from).size == 2 =>
      state
        .updated(from, Set())
        .updated(low, state(low) + state(from).min)
        .updated(high, state(high) + state(from).max)
    case Grab(bot, chip) =>
      state.updated(bot, state(bot) + chip)
    case default => state
  }

  // Parse instructions
  val grab: Regex =
    """value ([0-9]+) goes to bot ([0-9]+)""".r
  val give: Regex = """bot ([0-9]+) gives low to (bot|output) ([0-9]+) and high to (bot|output) ([0-9]+)""".r

  val instructions: List[Instruction] = input.map {
    case give(from, lowType, low, highType, high) => Give(Bot(from.toInt),
      if (lowType == "bot") Bot(low.toInt) else Output(low.toInt),
      if (highType == "bot") Bot(high.toInt) else Output(high.toInt))
    case grab(chip, bot) => Grab(Bot(bot.toInt), chip.toInt)
  }

  // Define solution
  def solution(stateMap: StateMap): Option[(Target, Int)] = for (
    part1 <- stateMap.find(target => target._2 == Set(61, 17)).map(_._1);
    nrs = Set(Output(0), Output(1), Output(2)).flatMap(stateMap(_));
    part2 = nrs.product if nrs.size == 3
  ) yield (part1, part2)

  // And solve!
  def solve(state: StateMap): (Target, Int) = {
    // scanLeft cause it keeps the intermediate states and the part 1 bot may have handed off the chips
    // by the time we get to the end of the instructions
    val states: Seq[StateMap] = instructions.scanLeft(state)(followInstruction)
    states.flatMap(solution).headOption.getOrElse(solve(states.last))
  }

  val initialState: StateMap = Map().withDefaultValue(Set())
  solve(initialState)
}