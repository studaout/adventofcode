package studaout.y2016

import studaout._

/*
how many of the listed triangles are possible(1: by line, 2) by column)?
 */

object Day3 {
  val inputFile = "/2016/day3-input.txt"

  def part1(): Unit = {
    val lns: Iterator[String] = lines(inputFile)
    val count = lns.foldLeft(0)( (c, l) => {
      val a = l.trim.split("\\s+").map( s => s.toInt)
      if ( a(0) + a(1) > a(2) && a(0) + a(2) > a(1) && a(1) + a(2) > a(0)) c + 1 else c
    })
    println(count)
  }

  def part2(): Unit = {
    val lns: Iterator[String] = lines(inputFile)
    val res = lns.foldLeft( (0,0, Array.ofDim[Int](3, 3)) ) ((state, line)=> { //index, count, array of 3 lines
      val a = line.trim.split("\\s+").map( s => s.toInt)
      state._3(state._1) = a
      val index = state._1 + 1
      if ( index == 3 ) {
        val cnt = columnBaseCount(state._3)
        (0, state._2+cnt, Array.ofDim[Int](3, 3))
      } else {
        (index, state._2, state._3)
      }
    })
    println(res._2)
  }

  def columnBaseCount(arr: Array[Array[Int]]) : Int = {
    def isTriangle(i:Int, count:Int) : Int = {
      val c = if ( arr(0)(i) + arr(1)(i) > arr(2)(i) && arr(0)(i) + arr(2)(i) > arr(1)(i) && arr(1)(i) + arr(2)(i) > arr(0)(i)) 1 else 0
      if ( i < 2 ) isTriangle(i+1, count+c) else c + count
    }
    isTriangle(0, 0)
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }
}
