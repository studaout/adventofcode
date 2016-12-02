package studaout.y2015

import studaout._


object Day2 {

  val inputFile = "/2015/day2-input.txt"

  def main(args: Array[String]) {
    val res = lines(inputFile).foldLeft( (0, 0))( (i, line) => {
      val d = line.split("x").map(s => s.toInt)
      val l = Array(d(0)*d(1), d(0)*d(2), d(1)*d(2))
      val m = 2 * l.sum  + l.min
      val paper = i._1 + m
      val sl = d.sorted
      val r = 2* (sl(0) + sl(1)) + sl.product
      val ribbon = i._2 + r
      (paper, ribbon)
    })
    println("sum paper: " + res._1)
    println("sum ribbon: " + res._2)
  }
}
