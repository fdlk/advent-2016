import scala.io.Source.fromInputStream
import scala.util.parsing.combinator.JavaTokenParsers

object day25 {

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

    def out: Parser[Instruction] = "out" ~> register ^^ { arg => r: Regs => print(r.read(arg.toString)); r.jmp(1) }

    def instruction: Parser[Instruction] = cpy | inc | dec | jnz | out
  }

  object InstructionParser extends InstructionParser

  def step(r: Regs): Regs = {
    program(r.ip)(r)
  }

  def execute(program: List[Instruction], initialState: Regs) = {
    Stream.iterate(initialState)(step).take(1000000).toList
  }

  val input: List[String] =
    fromInputStream(getClass.getResourceAsStream("day25.txt")).getLines.toList

  val program: List[Instruction] = input.map(line => InstructionParser.parseAll(InstructionParser.instruction, line).get)

  val binarySolution: String = "010101010101"
  val aToEnter = Integer.parseInt(binarySolution.reverse, 2) - offset

  val offset = 362 * 7
  val a = aToEnter
  val output = Integer.toBinaryString(offset + a).toList.reverse.mkString

  val initialState = Regs(0, offset + a, 2, offset + a, 15)
  execute(program,initialState)
}