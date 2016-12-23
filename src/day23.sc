import scala.annotation.tailrec
import scala.io.Source.fromInputStream
import scala.util.parsing.combinator.JavaTokenParsers

object day23 {

  case class Regs(a: Long, b: Long, c: Long, d: Long, ip: Int, program: List[String]) {
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
    override def toString: String = (a,b,c,d,ip).toString
  }

  sealed trait Instruction {
    def process(r: Regs): Regs
  }

  case class Cpy(from: String, to: Char) extends Instruction{
    override def process(r: Regs): Regs = r.cpy(to, from)
  }

  case class Inc(ab: Char) extends Instruction {
    override def process(r: Regs): Regs = r.update(ab, _ + 1)
  }

  case class Dec(ab: Char) extends Instruction {
    override def process(r: Regs): Regs = r.update(ab, _ - 1)
  }

  case class Jnz(arg: String, offset: String) extends Instruction {
    override def process(r: Regs): Regs = r.jmp(if (r.read(arg) != 0) r.read(offset).toInt else 1)
  }

  case class Tgl(arg: String) extends Instruction {
    override def process(r: Regs): Regs = {
      val lineNumber: Long = r.read(arg) + r.ip
      if(!r.program.indices.map(_.toLong).contains(lineNumber)) r.jmp(1)
      else {
        val n: Int = lineNumber.toInt
        val toggledInstruction: String = toggle(r.program(n))
        r.copy(program = r.program.updated(n,toggledInstruction)).jmp(1)
      }
    }
  }

  def toggle(s: String): String = s.split("""\s""").toList match {
    case "inc" :: arg :: Nil => List("dec", arg).mkString(" ")
    case _ :: arg :: Nil => List("inc", arg).mkString(" ")
    case "jnz" :: arg1 :: arg2 :: Nil => List("cpy", arg1, arg2).mkString(" ")
    case _ :: arg1 :: arg2 :: Nil => List("jnz", arg1, arg2).mkString(" ")
  }

  class InstructionParser extends JavaTokenParsers {
    def register: Parser[Char] = ("a" | "b" | "c" | "d") ^^ {_.charAt(0)}
    def offset: Parser[Int] = opt("+") ~> wholeNumber ^^ {_.toInt}
    def operand: Parser[String] = ("a" | "b" | "c" | "d" | offset) ^^ {_.toString}
    def cpy: Parser[Instruction] = "cpy" ~> operand ~ register ^^ { case from ~ to => Cpy(from, to) }
    def inc: Parser[Instruction] = "inc" ~> register ^^ { arg => Inc(arg) }
    def dec: Parser[Instruction] = "dec" ~> register ^^ { arg => Dec(arg) }
    def jnz: Parser[Instruction] = "jnz" ~> operand ~ operand ^^ { case arg ~ offset => Jnz(arg, offset) }
    def tgl: Parser[Instruction] = "tgl" ~> operand ^^ { arg => Tgl(arg)}
    def instruction: Parser[Instruction] = cpy | inc | dec | jnz | tgl
  }

  object InstructionParser extends InstructionParser

  def parse(instruction: String): InstructionParser.ParseResult[Instruction] =
    InstructionParser.parseAll(InstructionParser.instruction, instruction)

  def step(r: Regs): Regs = parse(r.program(r.ip)).map(_.process(r)).getOrElse(r.jmp(1))

  val program: List[String] =
    fromInputStream(getClass.getResourceAsStream("day23.txt")).getLines.toList

  val initialState: Regs = Regs(7,0,0,0,0,program)

  Stream.iterate(initialState)(step).find(r => !program.indices.contains(r.ip)).get.a

  val part1: Int = (1 to 7).product + 76 * 80

  val part2: Int = (1 to 12).product + 76 * 80
}