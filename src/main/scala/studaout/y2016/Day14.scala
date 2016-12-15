package studaout.y2016

import java.security.MessageDigest
import java.util.concurrent.atomic.AtomicInteger

import studaout._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object Day14 {

  val inputFile = "/2016/day14-input.txt"

  val code: String = firstLine(inputFile)

  implicit val ec = ExecutionContext.global

  /**
    * Considering future hashes for five-of-a-kind sequences does not cause those hashes to be skipped; instead, regardless
    * of whether the current hash is a key, always resume testing for keys starting with the very next hash.
    * For example, if the pre-arranged salt is abc:
    * The first index which produces a triple is 18, because the MD5 hash of abc18 contains ...cc38887a5....
    * However, index 18 does not count as a key for your one-time pad, because none of the next thousand hashes
    * (index 19 through index 1018) contain 88888.
    * The next index which produces a triple is 39; the hash of abc39 contains eee.
    * It is also the first key: one of the next thousand hashes (the one at index 816) contains eeeee.
    * None of the next six triples are keys, but the one after that, at index 92, is: it contains 999 and index 200 contains 99999.
    * Eventually, index 22728 meets all of the criteria to generate the 64th key.
    *
    * So, using our example salt of abc, index 22728 produces the 64th key.
    * Given the actual salt in your puzzle input, what index produces your 64th one-time pad key?
    */

  def part1(): Unit = {
    var hashes: List[String] = (0 to 999).map{ i=> generate(i) }.toList
    var cnt = 0
    var i = 0
    var gi = 1000
    while ( cnt < 64 ) {
      val str = hashes.head
      hashes = hashes.tail ::: List(generate(gi))
      gi = gi + 1
      val c = get3(str)
      if ( c != ' ' ) {
        val check = "" + c + c + c + c + c
        hashes.find( h => get5(check, h) ).foreach { s =>
          cnt = cnt + 1
          println(s"found $cnt = $str")
        }
      }
      i = i + 1
    }
    println(i-1)
  }

  /**
    * To implement key stretching, whenever you generate a hash, before you use it, you first find the MD5 hash of that hash,
    * then the MD5 hash of that hash, and so on, a total of 2016 additional hashings.
    * Always use lowercase hexadecimal representations of hashes.
    * The rest of the process remains the same, but now the keys are entirely different.
    * Given the actual salt in your puzzle input and using 2016 extra MD5 calls of key stretching,
    * what index now produces your 64th one-time pad key
    */
  def part2(): Unit = {
    val counter = new AtomicInteger(0)
    var hashes: List[String] = (0 to 999).map { i => Future {  generateMulti(i) } }.map { f =>
      Await.ready(f, Duration.Inf)
      f.value.get.get
    }.toList
    var cnt = 0
    var i = 0
    var gi = 1000
    while (cnt < 64) {
      val str = hashes.head
      hashes = hashes.tail ::: List(generateMulti(gi))
      gi = gi + 1
      val c = get3(str)
      if (c != ' ') {
        val check = "" + c + c + c + c + c
        hashes.find(h => get5(check, h)).foreach { s =>
          cnt = cnt + 1
          println(s"found $cnt = $str")
        }
      }
      i = i + 1
    }
    println(i - 1)
  }

  def generate(i: Int) : String = {
    var str = code + i
    MessageDigest.getInstance("MD5").digest(str.getBytes).map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}
  }

  def generateMulti(i: Int) : String = {
    var str = code + i
    for ( j <- 0 to 2016 ) {
      str = MessageDigest.getInstance("MD5").digest(str.getBytes).map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}
    }
    str
  }

  def get3(str: String) : Char = {
    for ( i <- 2 until str.length ) {
      val c = str.charAt(i)
      if ( str.charAt(i-2) == c && str.charAt(i-1) == c ) return c
    }
    ' '
  }

  def get5(str: String, hash: String) : Boolean = {
    hash.indexOf(str) != -1
  }


  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

}
