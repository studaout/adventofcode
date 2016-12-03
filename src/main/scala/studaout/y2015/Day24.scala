package studaout.y2015

import studaout._

/**
  * Created by timout on 12/28/15.
  */
object Day24 {

  val inputFile = "/2015/day24-input.txt"
  val lineList: List[Int] = lines(inputFile).map { _.toInt  }.toList
  val array: Array[Int] = lineList.toArray.sorted.reverse
  val num1 = 3
  val groupWeight1: Int = array.sum / num1

  val num2 = 4
  val groupWeight2: Int = array.sum / num2

  class MinSQECounter extends Function1[Set[Int], Int] with PartialFunction[Set[Int], Int]  {
    private var min = Int.MaxValue
    private var sqe:Long = Long.MaxValue //smallest quantum entanglement

    override def apply(set:Set[Int]) : Int = {
      val size = set.size
      if ( size < min ) {
        min = size
        sqe = set.foldLeft(1L){(p,n) => p*n }
        println("new: " + size + " sqe: " + sqe + " set: " + set.mkString(" "))
      } else if ( size == min ) {
        val setSQE:Long = set.foldLeft(1L){(p,n) => p*n }
        if ( setSQE < sqe) sqe = setSQE
        println("new: " + size + " sqe: " + sqe + " set: " + set.mkString(" "))
      }
      min
    }

    override def isDefinedAt(set:Set[Int]): Boolean = set.size <= min

    def get: Long = sqe
  }

  def part1(): Unit = {
    val counter = new MinSQECounter()
    println(" weigth1: " + groupWeight1)
    rec1(0, Set(), 0,counter)
    println(counter.get)
  }

  def rec1(sum: Int, set:Set[Int], pos:Int, counter:MinSQECounter) : Unit = {
    if ( sum == groupWeight1 ) {
      if ( counter.isDefinedAt(set) && rec1_1(0, set)) {
        counter(set)
      }
    } else {
      for (i <- pos until array.length) {
        val ns = sum + array(i)
        if ( ns <= groupWeight1 ) {
          rec1(ns, set + array(i),i+1, counter)
        }
      }
    }
  }

  def rec1_1(sum: Int, set:Set[Int]) : Boolean = {
    var res = false
    for (i <- array.indices) {
      if ( (! res )&& (! set.contains(array(i)) ) ) {
        val ns = sum + array(i)
        if ( ns == groupWeight1 ) res = true else if ( ns < groupWeight1 ) res = rec1_1(ns, set)
      }
    }
    res
  }

  def rec2(sum: Int, set:Set[Int], pos:Int, counter:MinSQECounter) : Unit = {
    if ( sum == groupWeight2 ) {
      if ( counter.isDefinedAt(set) && rec2_1(0, 0, set)) {
        counter(set)
      }
    } else {
      for (i <- pos until array.length) {
        val ns = sum + array(i)
        if ( ns <= groupWeight2 ) {
          rec2(ns, set + array(i),i+1, counter)
        }
      }
    }
  }

  def rec2_1(sum: Int, pos:Int, set:Set[Int]) : Boolean = {
    var res = false
    for (i <- pos until array.length) {
      if ( (! res )&& (! set.contains(array(i)) ) ) {
        val ns = sum + array(i)
        if ( ns == groupWeight2 ) {
          res = rec2_2(0, 0, set+array(i))
        } else if ( ns < groupWeight2) {
          res = rec2_1(ns, i+1, set+array(i))
        }
      }
    }
    res
  }

  def rec2_2(sum: Int, pos: Int, set:Set[Int]) : Boolean = {
    var res = false
    for (i <- pos until array.length) {
      if ( (! res )&& (! set.contains(array(i)) ) ) {
        val ns = sum + array(i)
        if ( ns == groupWeight2 ) res = true else if ( ns < groupWeight2 ) res = rec2_2(ns, i+1, set)
      }
    }
    res
  }

  def part2(): Unit = {
    val counter = new MinSQECounter()
    println(" weigth2: " + groupWeight2)
    rec2(0, Set(), 0,counter)
    println(counter.get)
  }

  def main(args: Array[String]) {
    part1()
    part2()
  }

}
