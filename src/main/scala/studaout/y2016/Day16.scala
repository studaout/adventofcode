package studaout.y2016

import studaout._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object Day16 {

  val inputFile = "/2016/day16-input.txt"

  val init: String = firstLine(inputFile)

  val length1 = 272
  val length2 = 35651584

  def getStr(targetLength: Int) : String = {
    var l = init.length
    var a = init
    while ( l < targetLength ) {
      val b = a.reverse.map( ch => if ( ch == '0' ) '1' else '0' )
      a = a + '0' + b
      l = a.length
    }
    println(a.length)
    a.substring(0, targetLength)
  }

  /**
    * Start with an appropriate initial state (your puzzle input). Then, so long as you don't have enough data yet to fill the disk,
    * repeat the following steps:
    *  Call the data you have at this point "a".
    *  Make a copy of "a"; call this copy "b".
    *  Reverse the order of the characters in "b".
    *  In "b", replace all instances of 0 with 1 and all 1s with 0.
    *  The resulting data is "a", then a single 0, then "b".
    *
    * Repeat these steps until you have enough data to fill the desired disk.
    * once the data has been generated, you also need to create a checksum of that data. Calculate the checksum only for the data that
    * fits on the disk, even if you generated more data than that in the previous step.
    * The checksum for some given data is created by considering each non-overlapping pair of characters in the input data.
    * If the two characters match (00 or 11), the next checksum character is a 1. If the characters do not match (01 or 10),
    * the next checksum character is a 0. This should produce a new string which is exactly half as long as the original.
    * If the length of the checksum is even, repeat the process until you end up with a checksum with an odd length.
    *
    * The first disk you have to fill has length 272. Using the initial state in your puzzle input, what is the correct checksum?
    */

  def part1(): Unit = {
    var isOdd = false
    var checkSum = getStr(length1)
    while ( ! isOdd ) {
      var  i = 0
      var s = ""
      while ( i < checkSum.length-1 ) {
        s = s + (if ( checkSum.charAt(i) == checkSum.charAt(i+1) ) '1' else '0')
        i = i + 2
      }
      checkSum = s
      isOdd =  checkSum.length % 2 > 0
    }
    println(checkSum)
  }

  /**
    * The second disk you have to fill has length 35651584. Again using the initial state in your puzzle input,
    * what is the correct checksum for this disk?
    */

  def part2(): Unit = {
    var isOdd = false
    var checkSum = getStr(length2)
    while ( ! isOdd ) {
      var  i = 0
      val s = new StringBuilder()
      while ( i < checkSum.length-1 ) {
        val ch = if ( checkSum.charAt(i) == checkSum.charAt(i+1) ) '1' else '0'
        s.append(ch)
        i = i + 2
      }
      checkSum = s.toString()
      isOdd =  checkSum.length % 2 > 0
    }
    println(checkSum)
  }

  implicit val ec = ExecutionContext.global


  def calculateSum(s: Int, f: Int, str: String): String = {
    var  i = s
    val buf = new StringBuilder()
    while ( i < f ) {
      val ch = if ( str.charAt(i) == str.charAt(i+1) ) '1' else '0'
      buf.append(ch)
      i = i + 2
    }
    buf.toString()
  }

  def part2_p(): Unit = { //2 thread version, for fun
    var checkSum = getStr(length2)
    while ( checkSum.length % 2 == 0 ) {
      val size = checkSum.length/2
      checkSum = if ( size % 2 > 0 ) calculateSum(0, checkSum.length, checkSum) else {
        List(0,size).map( i => Future{ calculateSum(i,i+size, checkSum)}).map( f=> {
          Await.ready(f, Duration.Inf)
          f.value.get.get
        }).foldLeft("")(_ + _)
      }
    }
    println(checkSum)
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
    part2_p()
  }

}
