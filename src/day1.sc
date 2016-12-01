import scala.util.matching.Regex

object day1 {
  val input: List[String] = common.loadPackets(List("day1.txt")).head.split(", ").toList

  // The city grid
  sealed trait Direction {
    def turn(turn: Turn): Direction = this match {
      case North => if (turn == Left) West else East
      case East => if (turn == Left) North else South
      case South => if (turn == Left) East else West
      case West => if (turn == Left) South else North
    }
  }

  case class Location(x: Int, y: Int) {
    def move(direction: Direction): Location = direction match {
      case North => Location(x, y + 1)
      case East => Location(x + 1, y)
      case South => Location(x, y - 1)
      case West => Location(x - 1, y)
    }

    def distance(): Int = x + y
  }

  // the instructions
  sealed trait Instruction

  case object North extends Direction

  case object East extends Direction

  case object South extends Direction

  case object West extends Direction

  sealed trait Turn extends Instruction

  case object Left extends Turn

  case object Right extends Turn

  case object Move extends Instruction

  val commandPattern: Regex = """([LR])(\d+)""".r

  def parseInstructions(instructionString: String): List[Instruction] = {
    val commandPattern(direction, steps) = instructionString
    val turn: Turn = direction match {
      case "L" => Left
      case "R" => Right
    }
    turn :: (1 to steps.toInt).map(_ => Move).toList
  }

  case class State(location: Location, facing: Direction, visited: Set[Location], firstVisited: Option[Int]) {
    def follow(instruction: Instruction): State = instruction match {
      case t: Turn => State(location, facing.turn(t), visited, firstVisited)
      case Move =>
        val newLocation = location.move(facing)
        val newFirstVisited = firstVisited.orElse(
          if (visited.contains(newLocation))
            Some(newLocation.distance())
          else
            Option.empty)
        State(newLocation, facing, visited + location, newFirstVisited)
    }

    def distance: Int = location.distance()
  }


  val commands: List[Instruction] = input.flatMap(parseInstructions)

  val initialState: State = State(Location(0, 0), North, Set(), Option.empty)
  val finish: State = commands.foldLeft(initialState)((state, command) => state.follow(command))
  finish.distance
  finish.firstVisited

}