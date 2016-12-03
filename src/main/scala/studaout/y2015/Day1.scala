package studaout.y2015

import studaout._

object Day1 {

  val inputFile = "/2015/day1-input.txt"
  val line: String = firstLine(inputFile)

  def main(args: Array[String]) {
    val res = line.foldLeft((0,0, false))( (i, c) => {
      val floor = c match {
        case '(' => i._1+1
        case ')' => i._1-1
      }
      val step = if (i._3) i._2 else i._2 + 1
      if ( i._3 || floor < 0) (floor, step, true) else (floor, step, false)
    })
    println("target floor: " + res._1)
    println("basement at step: " + res._2)
  }

}
