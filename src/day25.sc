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

  type Instruction = (Regs => (Regs, Option[Long]))

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

    def cpy: Parser[Instruction] = "cpy" ~> operand ~ register ^^ { case from ~ to => r: Regs => (r.cpy(to, from), None) }

    def inc: Parser[Instruction] = "inc" ~> register ^^ { arg => r: Regs => (r.update(arg, _ + 1), None) }

    def dec: Parser[Instruction] = "dec" ~> register ^^ { arg => r: Regs => (r.update(arg, _ - 1), None) }

    def jnz: Parser[Instruction] = "jnz" ~> operand ~ offset ^^ { case arg ~ offset => r: Regs => (r.jmp(if (r.read(arg) != 0) offset else 1), None) }

    def out: Parser[Instruction] = "out" ~> register ^^ { arg => r: Regs => (r.jmp(1), Some(r.read(arg.toString))) }

    def instruction: Parser[Instruction] = cpy | inc | dec | jnz | out
  }

  object InstructionParser extends InstructionParser

  val input: List[String] =
    fromInputStream(getClass.getResourceAsStream("day25.txt")).getLines.toList

  val program: List[Instruction] = input.map(line => InstructionParser.parseAll(InstructionParser.instruction, line).get)

  def step(regs: Regs): (Regs, Option[Long]) = program(regs.ip)(regs)

  def execute(input: Regs): Stream[Regs] = Stream.iterate(input)((r: Regs) => step(r)._1)

  def outputs(regs: Stream[Regs]): Stream[(Regs, Long)] =
    regs.map(r => step(r)).filter(_._2.isDefined).map(x => (x._1, x._2.get))

  def isValid(output: List[Long]): Boolean = output match {
    case Nil => true
    case 0 :: 1 :: rest => isValid(rest)
    case default => false
  }

  def isSolution(i: Int): Boolean = {
    val s: Stream[(Regs, Long)] = outputs(execute(Regs(i, 0, 0, 0, 0)))
    val head: (Regs, Long) = s.head
    val rest: List[Long] = s.tail.takeWhile(_ != head).map(_._2).toList
    isValid(head._2 :: rest)
  }

  Stream.from(1).find(isSolution)
}