package studaout.y2016

import studaout._

object Day8 {

  val inputFile = "/2016/day8-input.txt"

  /**
    * The magnetic strip on the card you swiped encodes a series of instructions for the screen; these instructions are your puzzle input.
    * The screen is 50 pixels wide and 6 pixels tall, all of which start off, and is capable of three somewhat peculiar operations:
    *  rect AxB turns on all of the pixels in a rectangle at the top-left of the screen which is A wide and B tall.
    *  rotate row y=A by B shifts all of the pixels in row A (0 is the top row) right by B pixels.
    *  Pixels that would fall off the right end appear at the left end of the row.
    *  rotate column x=A by B shifts all of the pixels in column A (0 is the left column) down by B pixels.
    *  Pixels that would fall off the bottom appear at the top of the column.
    *
    * There seems to be an intermediate check of the voltage used by the display: after you swipe your card,
    * if the screen did work, how many pixels should be lit?
    *
    * You notice that the screen is only capable of displaying capital letters; in the font it uses, each letter is 5 pixels wide and 6 tall.
    * After you swipe your card, what code is the screen trying to display?
    */
  def part1(): Unit = {
    val matrix = Array.ofDim[Char](50, 6)
    lines(inputFile).foreach {
      case r"rect (\d+)${w}x(\d+)${h}" =>
        for (x <- 0 until w.toInt) {
          for (y <- 0 until h.toInt) {
            matrix(x)(y) = '#'
          }
        }
      case r"rotate row y=(\d+)${y} by (\d+)${n}" =>
        val row = y.toInt
        for (i <- 0 until n.toInt) {
          var nch: Char = ' '
          for (j <- 0 until 50) {
            val cch = matrix(j)(row)
            matrix(j)(row) = nch
            if ( j == 49 ) {
              matrix(0)(row) = cch
            }
            nch = cch
          }
        }
      case r"rotate column x=(\d+)${x} by (\d+)${n}" =>
        val col = x.toInt
        for (i <- 0 until n.toInt) {
          var nch: Char = ' '
          for (j <- 0 until 6) {
            val cch = matrix(col)(j)
            matrix(col)(j) = nch
            if ( j == 5 ) {
              matrix(col)(0) = cch
            }
            nch = cch
          }
        }
    }
    var cnt = 0
    for (i <- 0 until 6) {
      for (j <- 1 to 50) {
        if ( matrix(j-1)(i) == '#' ) cnt = cnt+1
        print(matrix(j-1)(i))
        if ( j % 5 == 0 ) print("  ")
      }
      println()
    }
    println(cnt) //RURUCEOEIL
  }

  def part2(): Unit = {
    //
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

}
