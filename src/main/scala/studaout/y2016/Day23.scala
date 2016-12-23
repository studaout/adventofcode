package studaout.y2016

import studaout._

object Day23 {

  val inputFile = "/2016/day23-input.txt"

  trait Command
  case class CPY(v:Int, reg:String) extends Command
  case class CPYI(v1:Int, v2:Int) extends Command
  case class CPYR(reg1:String, reg2:String) extends Command
  case class CPYRI(reg:String, v2: Int) extends Command
  case class JNZ(reg:String, steps: Int) extends Command
  case class JNZI(v:Int, steps: Int) extends Command
  case class JNZIR(v:Int, regSteps:String) extends Command
  case class JNZR(reg1:String, reg2:String) extends Command
  case class INC(reg:String) extends Command
  case class DEC(reg:String) extends Command
  case class TGL(reg:String) extends Command

  def parse() : Array[Command] = {
    lines(inputFile).map {
      case r"cpy (-?\d+)${v} ([abcd]{1})${reg}" => CPY(v.toInt, reg)
      case r"cpy ([abcd]{1})${reg1} ([abcd]{1})${reg2}" => CPYR(reg1, reg2)
      case r"jnz ([abcd]{1})${reg} (-?\d+)${v}" => JNZ(reg, v.toInt)
      case r"jnz (-?\d+)${v} ([abcd]{1})${reg}" => JNZIR(v.toInt, reg)
      case r"jnz (-?\d+)${v} (-?\d+)${s}" => JNZI(v.toInt, s.toInt)
      case r"inc ([abcd]{1})${reg}" => INC(reg)
      case r"dec ([abcd]{1})${reg}" => DEC(reg)
      case r"tgl ([abcd]{1})${reg}" => TGL(reg)
    }.toArray
  }

  val commands: Array[Command] = parse()


  /**
    * After executing the assembunny code in your puzzle input, what value is left in register a?
    * day12 +
    * tgl x toggles the instruction x away (pointing at instructions like jnz does: positive means forward; negative means backward):
    *  For one-argument instructions, inc becomes dec, and all other one-argument instructions become inc.
    *    For two-argument instructions, jnz becomes cpy, and all other two-instructions become jnz.
    *    The arguments of a toggled instruction are not affected.
    *    If an attempt is made to toggle an instruction outside the program, nothing happens.
    *    If toggling produces an invalid instruction (like cpy 1 2) and an attempt is later made to execute that instruction, skip it instead.
    *    If tgl toggles itself (for example, if a is 0, tgl a would target itself and become inc a), the resulting instruction is not executed
    *    until the next time it is reached.
    *    Part1:
    *    Input a=7
    *    Output a=?
    *    Part2:
    *    Input a=12 + optimize execution time
    *    Output a=?
    */

  def execute(ci: Int, registers: Map[String, Int], commands: Array[Command]): Map[String, Int] = {
    print(s"$ci  ")
   registers.foreach( r => print(s"${r._1}=${r._2} "))
    println()
    val (next, nextState) = commands(ci) match {
      case CPY(v, reg) =>
        (ci + 1, registers + (reg ->  v))
      case CPYR(reg1, reg2) =>
        (ci + 1, registers + (reg2 ->  registers.getOrElse(reg1,0)))
      case CPYI(v1, v2) =>
        (ci + 1, registers)
      case CPYRI(reg, v2) =>
        (ci + 1, registers)
      case JNZ(reg, steps) =>
        if ( registers.getOrElse(reg,0) != 0 ) (ci + steps, registers) else (ci + 1, registers)
      case JNZI(v, steps) =>
        if ( v != 0 ) (ci + steps, registers) else (ci + 1, registers)
      case JNZIR(v, regSteps) =>
        val steps = registers.getOrElse(regSteps, 1)
        if ( v != 0 ) (ci + steps, registers) else (ci + 1, registers)
      case JNZR(reg1, reg2) =>
        val v = registers.getOrElse(reg1, 0)
        val steps = registers.getOrElse(reg2, 1) //TODO: default must be 0!!! - but than loop???
        if ( v != 0 ) (ci + steps, registers) else (ci + 1, registers)
      case INC(reg) =>
        (ci + 1, registers + (reg ->  (registers.getOrElse(reg,0)+1)))
      case DEC(reg) =>
        (ci + 1, registers + (reg ->  (registers.getOrElse(reg,0)-1)))
      case TGL(reg) =>
        val tglCI = ci + registers.getOrElse(reg, 0)
        if ( tglCI < commands.length && tglCI >= 0 ) {
          val tglCmd = commands(tglCI)
          tglCmd match {
            case CPY(v, r) => commands(tglCI) = JNZIR(v,r)
            case CPYR(r1, r2) => commands(tglCI) = JNZR(r1,r2)
            case CPYI(v1, v2) => commands(tglCI) = JNZI(v1,v2)
            case CPYRI(r, v2) => commands(tglCI) = JNZ(r,v2)
            case JNZ(r, steps) => commands(tglCI) = CPYRI(r, steps)
            case JNZI(v, steps) => commands(tglCI) = CPYI(v,steps)
            case JNZIR(v, steps) => commands(tglCI) = CPY(v,steps)
            case JNZR(r1,r2) => commands(tglCI) = CPYR(r1,r2)
            case INC(r) => commands(tglCI) = DEC(r)
            case DEC(r) => commands(tglCI) = INC(r)
            case TGL(r) => commands(tglCI) = INC(r)
          }
        }
        (ci+1, registers)
    }
    if ( next >= commands.length ) nextState else execute(next, nextState, commands)
  }


  def part1(): Unit = {
    val cmds = commands.toList.toArray
    val res = execute(0, Map("a"->7), cmds)
    res.foreach( r => println(s"${r._1}=${r._2}"))
    println("-----")
  }

  def part2(): Unit = { //non optimized
    val cmds = commands.toList.toArray
    val res = execute(0, Map("a"->12), cmds)
    res.foreach( r => println(s"${r._1}=${r._2}"))
    println("-----")
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

}
