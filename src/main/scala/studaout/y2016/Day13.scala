package studaout.y2016

import studaout._

object Day13 {

  val inputFile = "/2016/day13-input.txt"

  val code: Int = firstLine(inputFile).toInt

  /**
    *  Find x*x + 3*x + 2*x*y + y + y*y.
    *   Add the office designer's favorite number (your puzzle input).
    *   Find the binary representation of that sum; count the number of bits that are 1.
    *   If the number of bits that are 1 is even, it's an open space.
    *   If the number of bits that are 1 is odd, it's a wall.
    *   What is the fewest number of steps required for you to reach 31,39?
    */
  def part1(): Unit = {
    val tx = 31
    val ty = 39

    val mat: Array[Array[Int]] = new Array(100)
    for ( i <- 0 to 99 ) {
      mat(i) = new Array(100)
      for ( j <- 0 to 99 ) {
        mat(i)(j) = getCellType(i,j)
      }
    }
    var x = 0
    var y = 0
    var found = false
    var cur: List[(Int, Int)] = List((1,1))
    mat(1)(1) = 1
    while( ! found ) {
      cur = cur.flatMap{ c =>
        if ( c == (tx,ty) ) {
          found = true
          List()
        } else {
          val v = mat(c._1)(c._2) + 1
          List((c._1 - 1, c._2), (c._1 + 1, c._2), (c._1, c._2 - 1), (c._1, c._2 + 1))
            .filter { p =>
              if (p._1 >= 0 && p._1 < 100 && p._2 >= 0 && p._1 < 100 && mat(p._1)(p._2) == 0) {
                mat(p._1)(p._2) = v; true
              } else false
            }
        }
      }
    }
    println("-----found---")
    if ( found ) {
      var cnt = 0
      var c = (tx,ty)
      while ( found ) {
        val v = mat(c._1)(c._2)
        c = List( (c._1-1, c._2), (c._1+1, c._2), (c._1, c._2-1), (c._1, c._2+1)).find { p =>
          p._1 >= 0 && p._1 < 100 && p._2 >= 0 && p._1 < 100 && mat(p._1)(p._2) < v && mat(p._1)(p._2) > -1
        }.get
        cnt = cnt + 1
        if ( c == (1,1) ) found = false
      }
      println(cnt)
    } else {
      println("no way")
    }
  }

  /**
    * How many locations (distinct x,y coordinates, including your starting location) can you reach in at most 50 steps?
    */
  def part2(): Unit = {
    val size = 80
    val mat: Array[Array[Int]] = new Array(size)
    for ( i <- 0 until size ) {
      mat(i) = new Array(size)
      for ( j <- 0 until size ) {
        mat(i)(j) = getCellType(i,j)
      }
    }

    for ( i <- 0 until size ) {
      for ( j <- 0 until size ) {
        if ( mat(i)(j) == 0 ) print(0) else print('|')
      }
      println()
    }

    var cur: List[(Int, Int)] = List((1,1))
    mat(1)(1) = 1
    var v = 2
    while( v <= 51 ) {
      cur = cur.flatMap { c =>
        List((c._1 - 1, c._2), (c._1 + 1, c._2), (c._1, c._2 - 1), (c._1, c._2 + 1))
          .filter { p =>
            if (p._1 >= 0 && p._1 < size && p._2 >= 0 && p._1 < size && mat(p._1)(p._2) == 0) {
              mat(p._1)(p._2) = v
              true
            } else false
          }
      }
      v = v + 1
    }
    var cnt = 0
    for ( i <- 0 until size; j <- 0 until size ) {
      if ( mat(i)(j) > 0 ) cnt = cnt +1
    }
    println(cnt)
  }

  def getCellType(x: Int, y: Int) : Int = { //0 : openspace, -1 - wall
    val e = (x*x + 3*x + 2*x*y + y + y*y) + code
    val cnt = Integer.bitCount(e)
    if ( cnt%2 == 0 ) 0 else -1
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

}
