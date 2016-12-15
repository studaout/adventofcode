package studaout.y2016

import studaout._

object Day12 {

  /**
    * The assembunny code you've extracted operates on four registers (a, b, c, and d) that start at 0 and can hold any integer.
    * However, it seems to make use of only a few instructions:
    *  cpy x y copies x (either an integer or the value of a register) into register y.
    *  inc x increases the value of register x by one.
    *  dec x decreases the value of register x by one.
    *  jnz x y jumps to an instruction y away (positive means forward; negative means backward), but only if x is not zero.
    */

  val inputFile = "/2016/day12-input.txt"


  trait Command
  case class CPY(v:Int, reg:String) extends Command
  case class CPYR(reg1:String, reg2:String) extends Command
  case class JNZ(reg:String, steps: Int) extends Command
  case class JNZI(v:Int, steps: Int) extends Command
  case class INC(reg:String) extends Command
  case class DEC(reg:String) extends Command

  def parse() : Array[Command] = {
    lines(inputFile).map {
      case r"cpy (\d+)${v} ([abcd]{1})${reg}" => CPY(v.toInt, reg)
      case r"cpy ([abcd]{1})${reg1} ([abcd]{1})${reg2}" => CPYR(reg1, reg2)
      case r"jnz ([abcd]{1})${reg} (-?\d+)${v}" => JNZ(reg, v.toInt)
      case r"jnz (-?\d+)${v} (-?\d+)${s}" => JNZI(v.toInt, s.toInt)
      case r"inc ([abcd]{1})${reg}" => INC(reg)
      case r"dec ([abcd]{1})${reg}" => DEC(reg)
    }.toArray
  }

  val commands: Array[Command] = parse()


  /**
    * After executing the assembunny code in your puzzle input, what value is left in register a?
    */

  def execute(ci: Int, registers: Map[String, Int]): Map[String, Int] = {
    val (next, nextState) = commands(ci) match {
      case CPY(v, reg) =>
        (ci + 1, registers + (reg ->  v))
      case CPYR(reg1, reg2) =>
        (ci + 1, registers + (reg2 ->  registers.getOrElse(reg1,0)))
      case JNZ(reg, steps) =>
        if ( registers.getOrElse(reg,0) != 0 ) (ci + steps, registers) else (ci + 1, registers)
      case JNZI(v, steps) =>
        if ( v != 0 ) (ci + steps, registers) else (ci + 1, registers)
      case INC(reg) =>
        (ci + 1, registers + (reg ->  (registers.getOrElse(reg,0)+1)))
      case DEC(reg) =>
        (ci + 1, registers + (reg ->  (registers.getOrElse(reg,0)-1)))
    }
    if ( next >= commands.length ) nextState else execute(next, nextState)
  }

  def part1(): Unit = {
    val res = execute(0, Map())
    res.foreach( r => println(s"${r._1}=${r._2}"))
    println("-----")
  }

  /**
    * If you instead initialize register c to be 1, what value is now left in register a?
    */
  def part2(): Unit = {
    val res = execute(0, Map("c"->1))
    res.foreach( r => println(s"${r._1}=${r._2}"))
    println("-----")
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

}
