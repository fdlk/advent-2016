

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

  isValidCombination(Set("LG", "HM"))

  def isValid(state: State): Boolean = {
    val perFloor: Map[Int, Set[String]] = state.groupBy(_._2).mapValues(_.keySet)
    perFloor.values.forall(isValidCombination) && state("E") > 0 && state("E") < 5
  }

  def move(stateOption: Option[State], move: Move): Option[State] = stateOption match {
    case None => None
    case Some(state) => move match {
      case Move(items, direction) =>
        val newFloor: Int = if (direction == Up) state("E") + 1 else state("E") - 1
        val newState: State =
          items.foldLeft(state)((s, item) => s.updated(item, newFloor)).updated("E", newFloor)
        if (isValid(newState)) Some(newState) else None
    }
  }

  val initialState: Option[State] = Some(Map(("E", 1), ("HM", 1), ("LM", 1), ("HG", 2), ("LG", 3)))

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

  moves.scanLeft(initialState)(move)
}