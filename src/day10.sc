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

  case class Chip(nr: Int)

  case class Give(from: Bot, low: Target, high: Target) extends Instruction

  case class Grab(bot: Bot, value: Chip) extends Instruction

  case class Contents(chips: Set[Chip]) {
    def receive(chip: Chip) = Contents(chips + chip)

    def canGive: Boolean = chips.size == 2

    def low: Chip = chips.minBy(_.nr)

    def high: Chip = chips.maxBy(_.nr)
  }

  type StateMap = Map[Target, Contents]

  def followInstruction(state: StateMap, i: Instruction): StateMap = i match {
    case Give(from, low, high) if state(from).canGive =>
      state
        .updated(from, Contents(Set()))
        .updated(low, state(low).receive(state(from).low))
        .updated(high, state(high).receive(state(from).high))
    case Grab(bot, chip) =>
      state.updated(bot, state(bot).receive(chip))
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
    case grab(chip, bot) => Grab(Bot(bot.toInt), Chip(chip.toInt))
  }

  // Define solution
  def part1(stateMap: StateMap): Option[Target] =
    stateMap.find(target => target._2.chips == Set(Chip(61), Chip(17))).map(_._1)

  def part2(stateMap: StateMap): Option[Int] = {
    val nrs: Set[Int] = Set(Output(0), Output(1), Output(2)).map(stateMap(_)).flatMap(_.chips).map(_.nr)
    if (nrs.size == 3) Some(nrs.product) else None
  }

  def solution(state: StateMap): Option[(Target, Int)] = for (
    part1 <- part1(state);
    part2 <- part2(state)
  ) yield (part1, part2)

  // And solve!
  def solve(state: StateMap): (Target, Int) = {
    // scanLeft cause it keeps the intermediate states and the part 1 bot may have handed off the chips
    // by the time we get to the end of the instructions
    val states: Seq[StateMap] = instructions.scanLeft(state)(followInstruction)
    states.flatMap(solution).headOption.getOrElse(solve(states.last))
  }

  val initialState: StateMap = Map().withDefaultValue(Contents(Set()))
  solve(initialState)
}