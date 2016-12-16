package studaout.y2016

import studaout._

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

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

}
