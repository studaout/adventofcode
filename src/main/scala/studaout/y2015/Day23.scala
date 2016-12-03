package studaout.y2015

import studaout._

/**
  * Created by timout on 12/28/15.
  */
object Day23 {

  val inputFile = "/2015/day23-input.txt"

  def readFile(): List[Op with Product with Serializable] = {
    lines(inputFile).map {
      case r"hlf (a|b)${reg}" => Hlf(if ( reg == "a") 0 else 1)
      case r"tpl (a|b)${reg}" => Tpl(if ( reg == "a") 0 else 1)
      case r"inc (a|b)${reg}" => Inc(if ( reg == "a") 0 else 1)
      case r"jmp ([+-]{1}\d+)${offset}" => Jmp(offset.toInt)
      case r"jie (a|b)${reg}, ([+-]{1}\d+)${offset}" => Jie(if (reg == "a") 0 else 1, offset.toInt)
      case r"jio (a|b)${reg}, ([+-]{1}\d+)${offset}" => Jio(if (reg == "a") 0 else 1, offset.toInt)
      case default => throw new Exception("unknown: " + default )
    }.toList
  }

  case class State(registers:Array[Long], op:Int) {
    override def toString: String = {
      "op: " + op +", registers: " + registers.mkString(" ")
    }
  }

  trait Op extends Function1[State,State]

  case class Hlf(reg: Int) extends Op {
    override def apply(s: State): State = {
      s.registers(reg) = s.registers(reg) /2L
      State(s.registers, s.op+1)
    }
  }

  case class Tpl(reg: Int) extends Op {
    override def apply(s: State): State = {
      s.registers(reg) = s.registers(reg) * 3L
      State(s.registers, s.op+1)
    }
  }

  case class Inc(reg: Int) extends Op {
    override def apply(s: State): State = {
      s.registers(reg) +=1L
      State(s.registers, s.op+1)
    }
  }

  case class Jmp(offset: Int) extends Op {
    override def apply(s: State): State = {
      State(s.registers, s.op+offset)
    }
  }

  case class Jie(reg:Int, offset: Int) extends Op {
    override def apply(s: State): State = {
      if ( s.registers(reg) %2L == 0 )  State(s.registers, s.op+offset) else State(s.registers, s.op+1)
    }
  }

  case class Jio(reg:Int, offset: Int) extends Op {
    override def apply(s: State): State = {
      if ( s.registers(reg) == 1L )  State(s.registers, s.op+offset) else State(s.registers, s.op+1)
    }
  }

  def execute(state:State, ops:Array[Op]): State = {
    if ( state.op < 0 || state.op >= ops.length ) {
      state
    } else {
      val op = ops(state.op)
      val ns = op(state)
      execute(ns, ops)
    }
  }


  def part1(): Unit ={
    val ops = readFile()
    val init = State(Array[Long](0,0), 0)
    val res = execute(init, ops.toArray)
    println(res.registers(1))
  }

  def part2(): Unit ={
    val ops = readFile()
    val init = State(Array[Long](1,0), 0)
    val res = execute(init, ops.toArray)
    println(res.registers(1))
  }



  def main(args: Array[String]) {
    part1()
    part2()
  }

}
