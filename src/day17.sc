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

    def neighbors = for (direction <- List(up, down, left, right) if direction.isDefined) yield direction.get

    def finished(): Boolean = row == 3 && col == 3
  }

  def heuristicCostToGoal(s: State): Int = 0 //Dijkstra

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

  val initialState: State = State("", 0, 0)

  val solution = aStar(initialState)

  initialState.neighbors

  initialState.down.get.up.get.hash
}