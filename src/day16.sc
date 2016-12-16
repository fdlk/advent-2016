object day16 {

  def inverse(c: Char): Char = if (c == '0') '1' else '0'

  def dragon(a: String, desiredLength: Int): String =
    if (a.length > desiredLength) a.substring(0, desiredLength)
    else dragon(a + "0" + a.reverse.toList.map(inverse).mkString, desiredLength)

  def checksum(s: String): String = {
    if (s.length % 2 == 1) s
    else
      checksum(s.toList.grouped(2).map {
        case a :: b :: Nil if a == b => '1'
        case default => '0'
      }.mkString)
  }

  checksum(dragon("01000100010010111", 272))
  checksum(dragon("01000100010010111", 35651584))
}