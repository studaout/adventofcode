package studaout.y2015

import scala.collection.mutable
import studaout._

trait Expr {
  var p = false
  var evaluated = false
  var res = 0l

  def eval() : Long = {
    if ( p ) {
      throw new Exception("Loop")
    }
    if ( evaluated) {
      //println("cache: " + res)
      res
    } else {
      //println("Calculating: " + getClass.getName)
      p = true
      res = evalCur()
      evaluated = true
      p = false
      res
    }
  }
  def evalCur() : Long
}

case class None(str:String) extends Expr{
  override def evalCur(): Long = throw new Exception("Wrong expression: " + str)
}

case class Link(v:String)(implicit m: mutable.HashMap[String, Expr]) extends Expr {
  override def evalCur(): Long = {
    val e = m.getOrElse(v, None(v))
    e.eval()
  }
}

case class Num(v:Long) extends Expr { override def evalCur(): Long = v }
case class OR(e1:Expr, e2:Expr) extends Expr { override def evalCur(): Long = e1.eval() | e2.eval() }
case class AND(e1:Expr, e2:Expr) extends Expr { override def evalCur(): Long = e1.eval() & e2.eval() }
case class NOT(e:Expr) extends Expr { override def evalCur(): Long = ~ e.eval() }
case class LSHIFT(e:Expr, num:Expr) extends Expr { override def evalCur(): Long = e.eval() << num.eval() }
case class RSHIFT(e:Expr, num:Expr) extends Expr { override def evalCur(): Long = e.eval() >> num.eval() }

object Day7 {

  val inputFile = "/2015/day7-input.txt"
  val variable = "a"

  implicit val m = new mutable.HashMap[String, Expr]()

  def part1(): Unit = {
    lines(inputFile).foreach {
      case r"(.*)${rp} -> (.*)${v}" =>
        //println("var: " + v + "  value: " + rp)
        val expr = parse(rp)
        m.put(v, expr)
      case _ =>
    }

    m.get(variable).foreach( e=> {
      val res = e.eval()
      println("part 1: " + res)
    })
  }

  def part2(): Unit = {
    val bFromPart1 = 16076L
    val variable = "a"

    lines(inputFile).foreach {
      case r"(.*)${rp} -> (.*)${v}" =>
        val expr = parse(rp)
        m.put(v, expr)
      case _ =>
    }
    m.put("b", Num(bFromPart1))
    m.get(variable).foreach( e=> {
      val res = e.eval()
      println("part 2: " + res)
    })
  }

  def parse(str: String): Expr = str match {
    case r"(\d+)${n}" =>
      Num(n.toLong)
    case r"([a-z]+)${l}" =>
      Link(l)
    case r"(.*)${l} OR (.*)${r}" => OR(parse(l), parse(r))
    case r"(.*)${l} AND (.*)${r}" => AND(parse(l), parse(r))
    case r"(.*)${l} LSHIFT (.*)${r}" => LSHIFT(parse(l), parse(r))
    case r"(.*)${l} RSHIFT (.*)${r}" => RSHIFT(parse(l), parse(r))
    case r"NOT (.*)${l}" => NOT(parse(l))
    case _=> None(str)
  }

  def main(args: Array[String]) {
    part1()
    part2()
  }

}
