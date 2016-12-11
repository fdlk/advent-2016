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

  val initialState: State = Map(("E", 1), ("HM", 1), ("LM", 1), ("HG", 2), ("LG", 3))
  val finalState: State = Map(("E", 4), ("HM", 4), ("LM", 4), ("HG", 4), ("LG", 4))

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

  possibleMoves(initialState)
  val gen0 = Set(initialState)

  def solve(open: Set[State], closed: Set[State], steps: Int): Int = {
    println(steps)
    if (open.contains(finalState)) steps
    else {
      val next = nextGen(open, closed)
      solve(next, closed ++ next, steps + 1)
    }
  }

  solve(gen0, gen0, 0)

  val moves: List[Move] = List(
    Move(List("HM"), Up),
    Move(List("HG", "HM"), Up),
    Move(List("HM"), Down),
    Move(List("HM"), Down),
    Move(List("HM", "LM"), Up),
    Move(List("HM", "LM"), Up),
    Move(List("HM", "LM"), Up),
    Move(List("HM"), Down),
    Move(List("HG", "LG"), Up),
    Move(List("LM"), Down),
    Move(List("HM", "LM"), Up))


  val someInitialState: Option[State] = Some(initialState)

  def tryDoMove(s: Option[State], m: Move): Option[State] = {
    s.flatMap(x => doMove(x, m))
  }

  val solution: Seq[Option[State]] = moves.scanLeft(someInitialState)(tryDoMove)
}