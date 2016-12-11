object day11 {

  type State = Map[String, Int]

  sealed trait Direction

  object Up extends Direction {
    override def toString = "Up"
  }

  object Down extends Direction {
    override def toString = "Down"
  }

  case class Move(items: List[String], direction: Direction)

  def isValidCombination(combination: Set[String]): Boolean = {
    val generators = combination.filter(_.endsWith("G"))
    if (generators.isEmpty) true else {
      val chips = combination.filter(_.endsWith("M"))
      chips.forall(chip => generators.contains(chip.charAt(0) + "G"))
    }
  }

  def combinationsAreValid(state: State): Boolean = {
    val perFloor: Map[Int, Set[String]] = state.groupBy(_._2).mapValues(_.keySet)
    perFloor.values.forall(isValidCombination)
  }

  def isValid(state: State): Boolean = {
    state("E") > 0 && state("E") < 5 && combinationsAreValid(state)
  }

  def doMove(state: State, move: Move): Option[State] = move match {
    case Move(items, direction) =>
      val newFloor: Int = if (direction == Up) state("E") + 1 else state("E") - 1
      val newState: State = items.foldLeft(state)((s, item) => s.updated(item, newFloor)).updated("E", newFloor)
      if (isValid(newState)) Some(newState) else None
  }

  def possibleMoves(state: State): Set[Move] = {
    val currentFloor: Int = state("E")
    val candidates: List[String] = state.filter(item => item._2 == currentFloor && item._1 != "E").keys.toList

    val result: Iterator[Move] = for (
      items: List[String] <- candidates.combinations(1) ++ candidates.combinations(2);
      direction: Direction <- List(Up, Down)
    ) yield Move(items, direction)
    result.toSet
  }

  def nextGen(states: Set[State], visited: Set[State]): Set[State] = {
    val result: Set[State] = for (
      state: State <- states;
      move: Move <- possibleMoves(state);
      newState: State <- doMove(state, move)
    ) yield newState
    result -- visited
  }

  def solve(open: Set[State], closed: Set[State], steps: Int): Int = {
    println(steps, open.size)
    if (open.contains(finalState)) steps
    else {
      val next = nextGen(open, closed)
      solve(next, closed ++ next, steps + 1)
    }
  }

  /*
  The first floor contains a strontium generator, a strontium-compatible microchip, a plutonium generator, and a plutonium-compatible microchip.
The second floor contains a thulium generator, a ruthenium generator, a ruthenium-compatible microchip, a curium generator, and a curium-compatible microchip.
The third floor contains a thulium-compatible microchip.
The fourth floor contains nothing relevant.
   */

  val initialState: State = Map(
    ("E", 1), ("SG", 1), ("SM", 1), ("PG", 1), ("PM", 1),
    ("TG", 2), ("RG", 2), ("RM", 2), ("CG", 2), ("CM", 2),
    ("TM", 3)
  )
  val finalState: State = initialState.mapValues(_ => 4)

  val part1: Set[State] = Set(initialState)
  val result: Int = solve(part1, part1, 0)
}