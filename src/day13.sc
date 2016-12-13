object day13 {

  val favoriteNumber: Int = 1350
  val goal: Position = Position(31, 39)

  case class Position(x: Int, y: Int) {
    def up: Position = Position(x, y - 1)

    def down: Position = Position(x, y + 1)

    def left: Position = Position(x - 1, y)

    def right: Position = Position(x + 1, y)

    def valid: Boolean = x >= 0 && y >= 0

    def isFree: Boolean = valid &&
      java.lang.Integer.bitCount(x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber) % 2 == 0

    def finished: Boolean = this == goal

    def neighbors: Set[Position] = Set(up, down, left, right).filter(_.isFree)
  }

  type State = Position

  def heuristicCostToGoal(s: State): Int = (s.x - goal.x).abs + (s.y - goal.y).abs

  def aStar(start: State): Option[List[State]] = {
    aStarInternal(Set(), Set(start), Map(),
      Map[State, Int]().withDefault(s => Int.MaxValue).updated(start, 0),
      Map[State, Int]().withDefault(s => Int.MaxValue).updated(start, heuristicCostToGoal(start)))
  }

  @scala.annotation.tailrec
  def aStarInternal(closedSet: Set[State],
                    openSet: Set[State],
                    cameFrom: Map[State, State],
                    gScore: Map[State, Int],
                    fScore: Map[State, Int]): Option[List[State]] = {
    if (openSet.isEmpty) {
      None
    } else {
      val current = openSet.minBy(fScore.get)
      if (current.finished) {
        Some(reconstructPath(cameFrom, current, Nil))
      } else {
        val newGScores = (for (
          neighbor <- current.neighbors if !closedSet.contains(neighbor);
          tentativeGScore = gScore(current) + 1 // number of moves
          if !openSet.contains(neighbor) || tentativeGScore < gScore(neighbor)
        ) yield neighbor -> tentativeGScore)
          .toMap
          .withDefaultValue(Int.MaxValue)
        aStarInternal(closedSet + current,
          openSet - current ++ newGScores.keySet,
          cameFrom ++ newGScores.keySet.map(neighbor => neighbor -> current).toMap,
          gScore ++ newGScores,
          fScore ++ newGScores.map({ case (state, score) => (state, score + heuristicCostToGoal(state)) }))
      }
    }
  }

  def reconstructPath(cameFrom: Map[State, State], current: State, totalPath: List[State]): List[State] = {
    cameFrom
      .get(current)
      .map(s => reconstructPath(cameFrom, s, totalPath ++ List(current)))
      .getOrElse(totalPath)
  }

  val solution = aStar(Position(1, 1)).get
  solution.size
}