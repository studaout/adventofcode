package studaout.y2015

import scala.collection.mutable.ListBuffer
import studaout._

object Day9 {

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  def readfile() : (Int, ListBuffer[(Int, Int)]) = {
    val inputFile = "/2015/day9-input.txt"
    var size = 1
    var curSize = 1
    var curCity = ""
    var x = -1
    val list = new ListBuffer[(Int,Int)]()
    lines(inputFile).foreach {
      case r"(\S+)${s1} to (\S+)${s2} = (\d+)${d}" =>
        if (s1 != curCity) {
          if (curSize > size) size = curSize
          curSize = 1
          x += 1
          curCity = s1
        }
        curSize += 1
        list += ((x, d.toInt))
      case _ =>
    }
    (size, list)
  }

  def createMatrix(size:Int, list:ListBuffer[(Int, Int)]) : Array[Array[Int]] = {
    val mat =  Array.ofDim[Int](size, size)
    var x = 0
    var y = 1
    mat(0)(0) = -1
    for ( p <- list ) {
      if ( y == size ) {
        x +=1
        y = x+1
      }
      for ( i <- 0 until x ) mat(x)(i) = mat(i)(x)
      mat(x)(x) = -1
      mat(x)(y) = p._2
      y += 1
    }
    for ( i <- 0 until size ) mat(size-1)(i) = mat(i)(size-1)
    mat(size-1)(size-1) = -1
    mat
  }


  def part1(): Unit = {
    val (size, list) = readfile()
    val mat =  createMatrix(size, list)
    for ( x<-0 until size; y<-0 until size ) {
      print(mat(x)(y) + " ")
      if ( y == size -1 ) println()
    }
    var min = findMin(mat)
    println(min)
  }

  def findMin(mat: Array[Array[Int]]) : Int = {
    var steps = 0
    val size = mat.length
    var min = Integer.MAX_VALUE
    for ( y <- 0 until size ) {
      val state = new Array[Int](size)
      state(y) = 1
      var cm = findMin(y, 0, min, state, mat)
      if ( cm < min ) min = cm
    }
    min
  }

  def findMin(i: Int, sum: Int, min: Int, state: Array[Int], mat: Array[Array[Int]]) : Int = {
    val size = state.length
    var curMin = min
    var moved = false
    for ( y <- 0 until size ) {
      if ( state(y) == 0 ) {
        moved = true
        state(y) = 1
        val cs = sum + mat(i)(y)
        val cm = findMin(y, cs, min, state, mat)
        state(y) = 0
        if ( cm < curMin ) curMin = cm
      }
    }
    if ( moved ) curMin else {
      if ( min < sum ) min else sum
    }
  }

  def part2(): Unit = {
    val (size, list) = readfile()
    val mat =  createMatrix(size, list)
    var max = findMax(mat)
    println(max)
  }

  def findMax(mat: Array[Array[Int]]) : Int = {
    var steps = 0
    val size = mat.length
    var max = Integer.MIN_VALUE
    for ( y <- 0 until size ) {
      val state = new Array[Int](size)
      state(y) = 1
      var cm = findMax(y, 0, max, state, mat)
      if ( cm > max ) max = cm
    }
    max
  }

  def findMax(i: Int, sum: Int, max: Int, state: Array[Int], mat: Array[Array[Int]]) : Int = {
    val size = state.length
    var curMax = max
    var moved = false
    for ( y <- 0 until size ) {
      if ( state(y) == 0 ) {
        moved = true
        state(y) = 1
        val cs = sum + mat(i)(y)
        val cm = findMax(y, cs, max, state, mat)
        state(y) = 0
        if ( cm > curMax ) curMax = cm
      }
    }
    if ( moved ) curMax else {
      if ( max > sum ) max else sum
    }
  }

  def main(args: Array[String]) {
    part1()
    part2()
  }
}
