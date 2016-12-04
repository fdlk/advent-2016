import scala.io.Source._

object day4 {
  val lines: List[String] = fromInputStream(getClass.getResourceAsStream("day4.txt")).getLines.toList

  case class Room(name: String, sectorID: Int, checksum: String) {
    def leterFrequencySort(a: (Char, Int), b: (Char, Int)): Boolean = {
      if (a._2 == b._2) a._1.toInt < b._1.toInt else a._2 > b._2
    }

    def computedChecksum: String = {
      val charCounts = name.toList.filter(_ != '-').groupBy(x => x).mapValues(_.size)
      charCounts.toSeq.sortWith(leterFrequencySort).take(5).map(_._1).mkString
    }

    def isReal: Boolean = computedChecksum == checksum

    def rotX(c: Char): Char = ((c.toInt - 'a'.toInt + sectorID) % 26 + 'a'.toInt).toChar

    def decryptedName: String = {
      name.toList.map(c => if (c == '-') ' ' else rotX(c)).mkString
    }
  }

  def parseRoom(line: String): Room = {
    val roomFormat = """([a-z\-]+)-([0-9]+)\[([a-z]+)\]""".r
    val roomFormat(name, sectorID, checksum) = line
    Room(name, sectorID.toInt, checksum)
  }

  val realRooms: List[Room] = lines.map(parseRoom).filter(_.isReal)

  realRooms.map(_.sectorID).sum

  realRooms.map(r => (r.decryptedName, r.sectorID))
  // then text-search through the output to find something concerning the north pole
}