import scala.io.Source.fromInputStream
import scala.util.parsing.combinator.JavaTokenParsers

object day12 {

  case class Regs(a: Long, b: Long, c: Long, d: Long, ip: Int) {
    def update(ab: Char, f: Long => Long): Regs = ab match {
      case 'a' => copy(a = f(a)).jmp(1)
      case 'b' => copy(b = f(b)).jmp(1)
      case 'c' => copy(c = f(c)).jmp(1)
      case 'd' => copy(d = f(d)).jmp(1)
    }

    def cpy(to: Char, from: Char): Regs = update(to, (_) => read(from))

    def jmp(offset: Int) = copy(ip = ip + offset)

    def read(ab: Char): Long = ab match {
      case 'a' => a
      case 'b' => b
      case 'c' => c
      case 'd' => d
    }
  }

  type Instruction = (Regs => Regs)

  class InstructionParser extends JavaTokenParsers {
    def register: Parser[Char] = ("a" | "b" | "c" | "d") ^^ {
      _.charAt(0)
    }

    def offset: Parser[Int] = opt("+") ~> wholeNumber ^^ {
      _.toInt
    }

    def cpy = "cpy" ~> register ~ register ^^ { case from ~ to => r: Regs => r.cpy(to, from) }

    def cpr = "cpy" ~> offset ~ register ^^ { case value ~ ab => r: Regs => r.update(ab, (_) => value.toLong) }

    def inc = "inc" ~> register ^^ { ab => r: Regs => r.update(ab, _ + 1) }

    def dec = "dec" ~> register ^^ { ab => r: Regs => r.update(ab, _ - 1) }

    def jnv = "jnz" ~> offset ~ offset ^^ { case x ~ offset => r: Regs => r.jmp(if (x != 0) offset else 1) }

    def jnz = "jnz" ~> register ~ offset ^^ { case ab ~ offset => r: Regs => r.jmp(if (r.read(ab) != 0) offset else 1) }

    def instruction: Parser[Instruction] = cpr | cpy | inc | dec | jnz | jnv
  }

  object InstructionParser extends InstructionParser

  def execute(program: List[Instruction], initialState: Regs): Regs = {
    Stream.iterate(initialState)(r => program(r.ip)(r))
      .find(r => !program.indices.contains(r.ip)).get
  }

  val program: List[Instruction] = fromInputStream(getClass.getResourceAsStream("day12.txt")).getLines.toList
    .map(line => InstructionParser.parseAll(InstructionParser.instruction, line).get)
  execute(program, Regs(0, 0, 0, 0, 0))
  execute(program, Regs(0, 0, 1, 0, 0))
}