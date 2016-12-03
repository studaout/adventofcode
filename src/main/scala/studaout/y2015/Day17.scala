package studaout.y2015

import studaout._

object Day17 {

  val inputFile = "/2015/day17-input.txt"

  case class Acc(init: Int) extends Function0[Long] {
    private var count: Long = init

    override def apply(): Long = {
      count += 1
      count
    }

    def get = count
  }

  case class Acc2(init: Int) extends Function1[Int,Long] {
    private var cnt: Long = init
    private var min = Int.MaxValue

    override def apply(size:Int): Long = {
      if (size < min) {
        min = size
        cnt = 1
      } else if (size == min) {
        cnt += 1
      }
      cnt
    }

    def count = cnt
    def size = min
  }


  def part1(): Unit ={
    val arr = lines(inputFile).map {
      case r"(\d+)${size}" => size.toInt
      case default => throw new Exception("unknown: " + default )
    }.toArray.sorted
    val acc = Acc(0)
    sum(0, 0, arr, acc)
    println(acc.get)
  }

  def sum(curSum: Int, i: Int, arr: Array[Int], acc: Acc): Unit = {
    if ( curSum == 150 ) acc() else if (curSum < 150) {
      for ( p <- i until arr.length ) {
        //println(p + " : " + curSum)
        sum(curSum+arr(p), p+1, arr, acc)
      }
    }
  }


  def part2(): Unit ={

    val arr = lines(inputFile).map {
      case r"(\d+)${size}" => size.toInt
      case default => throw new Exception("unknown: " + default )
    }.toArray.sorted
    val acc = Acc2(0)
    sum2(0, 0, 0, arr, acc)
    printf("min: %d, count: %d \n  ", acc.size, acc.count)
  }

  def sum2(curSum: Int, i: Int, size:Int, arr: Array[Int], acc: Acc2): Unit = {
    if ( curSum == 150 ) {
      acc(size)
    } else if (curSum < 150) {
      for ( p <- i until arr.length ) {
        //println(p + " : " + curSum)
        sum2(curSum+arr(p), p+1, size+1, arr, acc)
      }
    }
  }

  def main(args: Array[String]) {
    part1()
    part2()
  }

}
