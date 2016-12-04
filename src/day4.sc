import scala.io.Source._

object day4 {
  val lines: List[String] = fromInputStream(getClass.getResourceAsStream("day4.txt")).getLines.toList

  case class Room(name: String, sectorID: Int, checksum: String) {

    def computedChecksum: String = {
      val charCounts = name.toList.filter(_ != '-').groupBy(identity).mapValues(_.size)
      charCounts.toSeq.sortWith(Room.letterFrequencySort).take(5).map(_._1).mkString
    }

    def isReal: Boolean = computedChecksum == checksum

    def decryptedName: String = {
      name.toList.map(c => if (c == '-') ' ' else Room.rotX(c, sectorID)).mkString
    }
  }

  object Room {
    def parse(line: String): Room = {
      val roomFormat = """([a-z\-]+)-([0-9]+)\[([a-z]+)\]""".r
      val roomFormat(name, sectorID, checksum) = line
      Room(name, sectorID.toInt, checksum)
    }

    def letterFrequencySort(a: (Char, Int), b: (Char, Int)): Boolean = {
      if (a._2 == b._2) a._1 < b._1 else a._2 > b._2
    }

    def rotX(c: Char, x:Int): Char = ((c.toInt - 'a'.toInt + x) % 26 + 'a'.toInt).toChar
  }

  val realRooms: List[Room] = lines.map(Room.parse).filter(_.isReal)

  realRooms.map(_.sectorID).sum

  realRooms.map(r => (r.decryptedName, r.sectorID)).filter(_._1.contains("pole"))
}