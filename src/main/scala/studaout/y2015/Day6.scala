package studaout.y2015

import studaout._

object Day6 {

  def part1(): Unit = {
    val inputFile = "/2015/day6-input.txt"
    val size = 1000
    val mat = Array.ofDim[Int](size, size)
    for ( line <- lines(inputFile) ) {
      line match {
        case r"turn off (\d+)${x1},(\d+)${y1} through (\d+)${x2},(\d+)${y2}" =>
          for ( x<-x1.toInt to x2.toInt; y<- y1.toInt to y2.toInt ) mat(x)(y) = 0
        case r"turn on (\d+)${x1},(\d+)${y1} through (\d+)${x2},(\d+)${y2}" =>
          for ( x<-x1.toInt to x2.toInt; y<- y1.toInt to y2.toInt ) mat(x)(y) = 1
        case r"toggle (\d+)${x1},(\d+)${y1} through (\d+)${x2},(\d+)${y2}" =>
          for ( x<-x1.toInt to x2.toInt; y<- y1.toInt to y2.toInt ) {
            val v = mat(x)(y)
            mat(x)(y) = if ( v == 0 ) 1 else 0
          }
        case _ =>
      }
    }
    var count = 0
    for ( x<-0 until size; y<-0 until size ) count += mat(x)(y)
    println(count)
  }

  def part2(): Unit = {
    val inputFile = "/2015/day6-input.txt"
    val size = 1000
    val mat = Array.ofDim[Int](size, size)
    lines(inputFile).foreach {
      case r"turn off (\d+)${x1},(\d+)${y1} through (\d+)${x2},(\d+)${y2}" =>
        for (x <- x1.toInt to x2.toInt; y <- y1.toInt to y2.toInt) {
          if (mat(x)(y) > 0) mat(x)(y) -= 1
        }
      case r"turn on (\d+)${x1},(\d+)${y1} through (\d+)${x2},(\d+)${y2}" =>
        for (x <- x1.toInt to x2.toInt; y <- y1.toInt to y2.toInt) mat(x)(y) += 1
      case r"toggle (\d+)${x1},(\d+)${y1} through (\d+)${x2},(\d+)${y2}" =>
        for (x <- x1.toInt to x2.toInt; y <- y1.toInt to y2.toInt) mat(x)(y) += 2
      case _ =>
    }
    var count = 0
    for ( x<-0 until size; y<-0 until size ) count += mat(x)(y)
    println(count)
  }

  def main(args: Array[String]) {
    part1()
    part2()
  }
}
