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

    def cpy(to: Char, from: String): Regs = update(to, (_) => read(from))

    def jmp(offset: Int): Regs = copy(ip = ip + offset)

    def read(arg: String): Long = arg match {
      case "a" => a
      case "b" => b
      case "c" => c
      case "d" => d
      case _ => arg.toLong
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

    def operand: Parser[String] = ("a" | "b" | "c" | "d" | offset) ^^ {
      _.toString
    }

    def cpy: Parser[Instruction] = "cpy" ~> operand ~ register ^^ { case from ~ to => r: Regs => r.cpy(to, from) }

    def inc: Parser[Instruction] = "inc" ~> register ^^ { arg => r: Regs => r.update(arg, _ + 1) }

    def dec: Parser[Instruction] = "dec" ~> register ^^ { arg => r: Regs => r.update(arg, _ - 1) }

    def jnz: Parser[Instruction] = "jnz" ~> operand ~ offset ^^ { case arg ~ offset => r: Regs => r.jmp(if (r.read(arg) != 0) offset else 1) }

    def instruction: Parser[Instruction] = cpy | inc | dec | jnz
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