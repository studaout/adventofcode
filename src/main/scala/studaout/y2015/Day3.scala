package studaout.y2015

import studaout._

object Day3 {

  val inputFile = "/2015/day3-input.txt"
  val line: String = firstLine(inputFile)
  val size: Int = line.length

  def part1() {
    val matrix = Array.ofDim[Int](size*2, size*2)
    matrix(size)(size) = 1
    val res = line.foldLeft((size, size, 1)) ( (i, c) => {
      val x = if ( c == '>') i._1 + 1 else if ( c == '<') i._1 - 1 else i._1
      val y = if ( c == 'v') i._2 + 1 else if ( c == '^') i._2 - 1 else i._2
      matrix(x)(y) += 1
      val count = if ( matrix(x)(y) == 1 ) i._3 + 1 else i._3
      (x,y,count)
    })
    println("part 1: " + res._3)
  }

  def part2() {
    val matrix = Array.ofDim[Int](size*2, size*2)
    matrix(size)(size) = 2
    val res = line.foldLeft((size, size, size, size, 0, 1)) ( (i, c) => {
      val pointer = i._5
      val x1 = if ( pointer == 0 && c == '>') i._1 + 1 else if ( pointer == 0 && c == '<') i._1 - 1 else i._1
      val x2 = if ( pointer == 1 && c == '>') i._3 + 1 else if ( pointer == 1 && c == '<') i._3 - 1 else i._3
      val y1 = if ( pointer == 0 && c == 'v') i._2 + 1 else if ( pointer == 0 && c == '^') i._2 - 1 else i._2
      val y2 = if ( pointer == 1 && c == 'v') i._4 + 1 else if ( pointer == 1 && c == '^') i._4 - 1 else i._4
      if ( pointer == 0 ) {
        matrix(x1)(y1) += 1
        val count = if ( matrix(x1)(y1) == 1 ) i._6 + 1 else i._6
        (x1,y1,x2,y2, 1, count)
      } else {
        matrix(x2)(y2) += 1
        val count = if ( matrix(x2)(y2) == 1 ) i._6 + 1 else i._6
        (x1,y1,x2,y2, 0, count)
      }
    })
    println("part 2: " + res._6)
  }


  def main(args: Array[String]) {
    part1()
    part2()
  }
}
