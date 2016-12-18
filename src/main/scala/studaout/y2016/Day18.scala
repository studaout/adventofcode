package studaout.y2016

import studaout.firstLine

object Day18 {

  val inputFile = "/2016/day18-input.txt"

  val init: String = firstLine(inputFile)

  def line(cnt: Int, recNum: Int, total: Int, prev: String): Int = {
    val size = prev.length
    val next = (0 until size).map {
      case j@0 =>
        if (prev.charAt(j + 1) == '^') '^' else '.'
      case j@p if p == (size-1) =>
        if (prev.charAt(j - 1) == '^') '^' else '.'
      case j@p =>
        if (
          (prev.charAt(j - 1) == '^' && prev.charAt(j) == '^' && prev.charAt(j + 1) == '.') ||
            (prev.charAt(j - 1) == '.' && prev.charAt(j) == '^' && prev.charAt(j + 1) == '^') ||
            (prev.charAt(j - 1) == '^' && prev.charAt(j) == '.' && prev.charAt(j + 1) == '.') ||
            (prev.charAt(j - 1) == '.' && prev.charAt(j) == '.' && prev.charAt(j + 1) == '^')
        ) '^' else '.'
    }.mkString
    if ( recNum == total-1 )
      cnt + next.filter( _ == '.').length
    else
      line(cnt+next.filter( _ == '.').length, recNum+1, total, next)
  }

  /**
    * Its left and center tiles are traps, but its right tile is not.
    * Its center and right tiles are traps, but its left tile is not.
    * Only its left tile is a trap.
    * Only its right tile is a trap.
    *  '&#94;' - trap
    *  '.' - safe
    *
    *  Starting with the map in your puzzle input, in a total of 40 rows (including the starting row), how many safe tiles are there?
    */
  def part1(): Unit = {
    println(line(init.filter(_ == '.').length, 1, 40, init))
  }

  /**
    * How many safe tiles are there in a total of 400000 rows?
    */
  def part2(): Unit = {
    println(line(init.filter(_ == '.').length, 1, 400000, init))
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

}
