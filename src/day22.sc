import scala.io.Source.fromInputStream
import scala.util.matching.Regex

object day22 {
  val input: List[String] = fromInputStream(getClass.getResourceAsStream("day22.txt")).getLines.toList.drop(2)

  case class Node(x: Int, y: Int, size: Int, used: Int, avail: Int, hasDesiredData: Boolean) {
    def isViableWith(b: Node): Boolean = used != 0 && b != this && used <= b.avail

    def toChar: Char = if (hasDesiredData) 'G' else if (used == 0) '_' else if (used > 100) '#' else '.'
  }

  val inputRegex:Regex ="""/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%""".r

  def parse(s: String): Node = s match {
    case inputRegex(x, y, size, used, avail, _) => Node(x.toInt, y.toInt, size.toInt, used.toInt, avail.toInt, x == "35" && y == "0")
  }

  val nodes: Set[Node] = input.map(parse).toSet
  val part1: Int = (for (a <- nodes; b <- nodes if a.isViableWith(b)) yield (a, b)).size

  case class SimpleNode(x: Int, y: Int, char: Char) {
    def isDone: Boolean = x == 0 && y == 0 && char == 'G'

    def liesNextTo(n: SimpleNode): Boolean = (n.x - x).abs + (n.y - y).abs == 1

    def emptied = SimpleNode(x, y, '_')

    def filled(c: Char) = SimpleNode(x, y, c)

    override def toString: String = char.toString
  }

  case class State(nodes: Set[SimpleNode]) {
    def validMoves: Set[(SimpleNode, SimpleNode)] =
      for (a <- nodes if a.char == '_'; b <- nodes if b.liesNextTo(a) && b.char != '#') yield (a, b)


    override def toString: String = {
      val grid: List[List[SimpleNode]] = nodes.toList.sortBy(_.x).sortBy(_.y).grouped(36).toList
      "\n" + grid.map(_.mkString).mkString("\n")
    }

    def neighbors: Set[State] = {
      for ((a, b) <- validMoves) yield State(nodes - a - b + b.emptied + a.filled(b.char))
    }

    def finished: Boolean = nodes.exists(_.isDone)

    def heuristicCostToGoal: Int = if (finished) 0 else {
      val dataNode = nodes.find(_.char == 'G').get
      val emptyNode = nodes.find(_.char == '_').get
      val distanceBetween = (emptyNode.x - dataNode.x).abs + (emptyNode.y - dataNode.y).abs
      (dataNode.x + dataNode.y) * 5 + (if (distanceBetween > 2) distanceBetween else 0)
    }
  }

  val initialState: State = State(nodes.map(n => SimpleNode(n.x, n.y, n.toChar)))

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

  val part2:Int = aStar(initialState).get.size
}