package studaout.y2016

import studaout._

object Day25 {

  /**
    * You open the door and find yourself on the roof. The city sprawls away from you for miles and miles.
    * There's not much time now - it's already Christmas, but you're nowhere near the North Pole, much too far to deliver these stars to the sleigh in time.
    * However, maybe the huge antenna up here can offer a solution. After all, the sleigh doesn't need the stars, exactly;
    * it needs the timing data they provide, and you happen to have a massive signal generator right here.
    * You connect the stars you have to your prototype computer, connect that to the antenna, and begin the transmission.
    * Nothing happens.
    * You call the service number printed on the side of the antenna and quickly explain the situation.
    * "I'm not sure what kind of equipment you have connected over there," he says, "but you need a clock signal."
    * You try to explain that this is a signal for a clock.
    * "No, no, a clock signal - timing information so the antenna computer knows how to read the data you're sending it.
    * An endless, alternating pattern of 0, 1, 0, 1, 0, 1, 0, 1, 0, 1...." He trails off.
    * You ask if the antenna can handle a clock signal at the frequency you would need to use for the data from the stars.
    * "There's no way it can! The only antenna we've installed capable of that is on top of a top-secret Easter Bunny installation,
    * and you're definitely not-" You hang up the phone.
    * You've extracted the antenna's clock signal generation assembunny code (your puzzle input);
    * it looks mostly compatible with code you worked on just recently.
    * This antenna code, being a signal generator, uses one extra instruction:
    *  out x transmits x (either an integer or the value of a register) as the next value for the clock signal.
    *  The code takes a value (via register a) that describes the signal to generate, but you're not sure how it's used.
    *  You'll have to find the input to produce the right signal through experimentation.
    *  What is the lowest positive integer that can be used to initialize register a and cause the code to output a clock signal
    *  of 0, 1, 0, 1... repeating forever?
    */

  val inputFile = "/2016/day25-input.txt"

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
  case class OUT(reg:String)extends Command

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
      case r"out ([abcd]{1})${reg}" => OUT(reg)
    }.toArray
  }

  val commands: Array[Command] = parse()

  def execute(ci: Int, registers: Map[String, Int], commands: Array[Command], outSig: String): String = {
    val (next, nextState, outStr) = commands(ci) match {
      case CPY(v, reg) =>
        (ci + 1, registers + (reg ->  v), outSig)
      case CPYR(reg1, reg2) =>
        (ci + 1, registers + (reg2 ->  registers.getOrElse(reg1,0)), outSig)
      case CPYI(v1, v2) =>
        (ci + 1, registers, outSig)
      case CPYRI(reg, v2) =>
        (ci + 1, registers, outSig)
      case JNZ(reg, steps) =>
        if ( registers.getOrElse(reg,0) != 0 ) (ci + steps, registers, outSig) else (ci + 1, registers, outSig)
      case JNZI(v, steps) =>
        if ( v != 0 ) (ci + steps, registers, outSig) else (ci + 1, registers, outSig)
      case JNZIR(v, regSteps) =>
        val steps = registers.getOrElse(regSteps, 1)
        if ( v != 0 ) (ci + steps, registers, outSig) else (ci + 1, registers, outSig)
      case JNZR(reg1, reg2) =>
        val v = registers.getOrElse(reg1, 0)
        val steps = registers.getOrElse(reg2, 1) //TODO: default must be 0!!!
        if ( v != 0 ) (ci + steps, registers, outSig) else (ci + 1, registers, outSig)
      case INC(reg) =>
        (ci + 1, registers + (reg ->  (registers.getOrElse(reg,0)+1)), outSig)
      case DEC(reg) =>
        (ci + 1, registers + (reg ->  (registers.getOrElse(reg,0)-1)), outSig)
      case OUT(reg) =>
        (ci + 1, registers , outSig + registers.getOrElse(reg,0).toString)
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
        (ci+1, registers, outSig)
    }
    if ( outStr.length == 10 || next >= commands.length ) outStr else execute(next, nextState, commands, outStr)
  }

  def execute2(i: Int, sampleSize: Int): String = {
    val buf = new StringBuilder("")
    val input = i + 2538
    def rec(a: Int): String = {
      if ( buf.size == sampleSize ) buf.toString() else {
        buf.append(a % 2)
        val n = if (a < 2) input else a / 2
        rec(n)
      }
    }
    rec(input)
  }

  def test(str:String): Boolean =  str.matches("(01)+")

  /**
    * Interpteter
    */
  def part1(): Unit = {
    var i = 1
    var isDone = false
    while ( ! isDone ) {
      println(s"testing $i")
      val str = execute(0, Map("a"->i), commands, "")
      isDone = test(str)
      if ( ! isDone ) i = i+1
    }
    println(i)
  }

  /**
    * Alternative solution
    */
  def part1_2(): Unit  ={
    def stream: Stream[Int] = {
      def loop(i: Int): Stream[Int] = {
        val str = execute2(i, 10)
        if ( test(str) ) i #:: Stream.empty else i #:: loop(i+1)
      }
      loop(1)
    }
    println(stream.last)
  }

  def part2(): Unit = {
  }

  def main(args: Array[String]): Unit = {
    part1_2()
    part2()
  }

}
