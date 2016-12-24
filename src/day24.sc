import scala.io.Source.fromInputStream

object day24 {
  val maze: List[String] = fromInputStream(getClass.getResourceAsStream("day24.txt")).getLines.toList

  case class Location(row: Int, col: Int) {
    def isWall: Boolean = maze(row).charAt(col) == '#'

    def neighbors: Set[Location] = Set(
      Location(row - 1, col),
      Location(row + 1, col),
      Location(row, col - 1),
      Location(row, col + 1)
    ).filter(!_.isWall)

    def distanceFrom(other: Location): Int = (row - other.row).abs + (col - other.col).abs
  }

  object Location {
    def find(c: Char): Location = {
      val row = maze.indexWhere(_.contains(c))
      val col = maze(row).indexOf(c)
      Location(row, col)
    }

    def distanceFrom(a: Char, b: Char): Int = aStar(State(find(a), find(b))).get.length
  }

  case class State(current: Location, goal: Location) {
    def heuristicCostToGoal: Int = current.distanceFrom(goal)

    def finished: Boolean = current == goal

    def neighbors: Set[State] = for (l <- current.neighbors) yield copy(current = l)
  }

  def aStar(start: State): Option[List[State]] = {
    aStarInternal(Set(), Set(start), Map(),
      Map[State, Int]().withDefault(s => Int.MaxValue).updated(start, 0),
      Map[State, Int]().withDefault(s => Int.MaxValue).updated(start, start.heuristicCostToGoal))
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
          fScore ++ newGScores.map({ case (state, score) => (state, score + state.heuristicCostToGoal) }))
      }
    }
  }

  def reconstructPath(cameFrom: Map[State, State], current: State, totalPath: List[State]): List[State] = {
    cameFrom
      .get(current)
      .map(s => reconstructPath(cameFrom, s, totalPath ++ List(current)))
      .getOrElse(totalPath)
  }

  val pois: List[Char] = (maze.map(_.toSet).reduce(_++_) - '.' - '#').toList

  val distances: Map[List[Char], Int] = (for (a <- pois; b <- pois) yield List(a, b) -> Location.distanceFrom(a, b)).toMap

  def cost(route: List[Char]): Int = route.sliding(2).map(distances).sum

  val part1: Int = pois.permutations.filter(_.head =='0').map(cost).min

  val part2: Int = pois.permutations.filter(_.head =='0').map(p => cost(p:::List('0'))).min
}