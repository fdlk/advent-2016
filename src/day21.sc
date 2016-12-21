import scala.io.Source.fromInputStream

object day21 {
  val input: List[String] = fromInputStream(getClass.getResourceAsStream("day21.txt")).getLines.toList

  val moveFromToRegex = """move position ([0-9]) to position ([0-9])""".r
  val rotateRightRegex = """rotate right ([0-9]) steps?""".r
  val rotateLeftRegex = """rotate left ([0-9]) steps?""".r
  val swapRegex = """swap position ([0-9]) with position ([0-9])""".r
  val swapLetterRegex = """swap letter ([a-z]) with letter ([a-z])""".r
  val reversePositionsRegex = """reverse positions ([0-9]) through ([0-9])""".r
  val rotateBased = """rotate based on position of letter ([a-z])""".r

  def process(password: String, instruction: String): String = instruction match {
    case moveFromToRegex(fromString: String, toString: String) => {
      val from: Int = fromString.toInt
      val to: Int = toString.toInt
      val removed: List[Char] = (password.toList.slice(0, from) ::: password.toList.slice(from + 1, password.size))
      (removed.slice(0, to) ::: List(password(from)) ::: removed.slice(to, removed.size)).mkString
    }
    case rotateRightRegex(nString: String) => rotateRightN(password, nString.toInt)
    case rotateLeftRegex(nString: String) => rotateLeftN(password, nString.toInt)
    case swapRegex(aString: String, bString: String) =>
      swap(password, aString.toInt, bString.toInt)
    case swapLetterRegex(aLetter: String, bLetter: String) =>
      swap(password, password.indexOf(aLetter), password.indexOf(bLetter))
    case reversePositionsRegex(aString: String, bString: String) => {
      val a: Int = aString.toInt
      val b: Int = bString.toInt
      List.range(0, password.length).map(_ match {
        case x if x < a || x > b => password(x)
        case x => password.substring(a, b + 1).reverse(x - a)
      }).mkString
    }
    case rotateBased(letter: String) => {
      val index: Int = password.indexOf(letter)
      val n: Int = 1 + index + (if (index >= 4) 1 else 0)
      rotateRightN(password, n)
    }
    case default => {
      println("cannot parse: " + instruction)
      password
    }
  }

  def rotateLeft(password: String): String = (password.toList.slice(1, password.length) ::: List(password(0))).mkString

  def rotateLeftN(password: String, n: Int): String = Function.chain(List.fill(n)(rotateLeft _))(password)

  def rotateRight(password: String): String = (password(password.length - 1) :: password.toList.slice(0, password.length - 1)).mkString

  def rotateRightN(password: String, n: Int): String = Function.chain(List.fill(n)(rotateRight _))(password)

  def swap(password: String, a: Int, b: Int): String = List.range(0, password.length).map(_ match {
    case x if x == a => password(b)
    case x if x == b => password(a)
    case x => password(x)
  }).mkString


  def scramble(password: String): String = input.foldLeft(password)(process)

  val part1: String = scramble("abcdefgh")

  val encrypted: String = "fbgdceah"
  val part2: String = (
    for (candidate <- encrypted.permutations
         if scramble(candidate) == encrypted)
      yield candidate).toList.head

  // tests
  swap("abcdef", 4, 0) == "ebcdaf"
  process("abcdef", "reverse positions 2 through 4") == "abedcf"
  process("abcdef", "reverse positions 1 through 1") == "abcdef"
  process("abcdef", "reverse positions 1 through 2") == "acbdef"
  process("abcdef", "reverse positions 0 through 5") == "fedcba"
  process("abcdef", "swap letter d with letter a") == "dbcaef"
  process("abcdef", "swap position 4 with position 0") == "ebcdaf"
  process("bcdea", "move position 1 to position 4") == "bdeac"
  process("abcde", "rotate right 1 step") == "eabcd"
  process("abcde", "rotate right 2 steps") == "deabc"
  process("abcde", "rotate left 1 step") == "bcdea"
  process("abcde", "rotate left 2 steps") == "cdeab"
  process("ecabd", "rotate based on position of letter d") == "decab"
}