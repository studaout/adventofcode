package studaout.y2016

import studaout._

object Day20 {

  val inputFile = "/2016/day20-input.txt"

  val min = 0L
  val max = 4294967295L

  val init: List[(Long, Long)] = lines(inputFile).map{ line =>
    val arr = line.split("-").map(_.toLong)
    (arr(0), arr(1))
  }.toList.sortBy(_._1)


  /**
    * You'd like to set up a small hidden computer here so you can use it to get back into the network later. However,
    * the corporate firewall only allows communication with certain external IP addresses.
    * You've retrieved the list of blocked IPs from the firewall, but the list seems to be messy and poorly maintained,
    * and it's not clear which IPs are allowed. Also, rather than being written in dot-decimal notation,
    * they are written as plain 32-bit integers, which can have any value from 0 through 4294967295, inclusive.
    * For example, suppose only the values 0 through 9 were valid, and that you retrieved the following blacklist:
    * 5-8
    * 0-2
    * 4-7
    * The blacklist specifies ranges of IPs (inclusive of both the start and end value) that are not allowed.
    * Then, the only IPs that this firewall allows are 3 and 9, since those are the only numbers not in any range.
    * Given the list of blocked IPs you retrieved from the firewall (your puzzle input), what is the lowest-valued IP that is not blocked?
    */

  def part1(): Unit = {
    var min = init.foldLeft(0L) ( (s, p) => if ( s >= p._1 && s <= p._2 ) p._2 + 1 else s )
    println(min)
  }

  /**
    * How many IPs are allowed by the blacklist?
    */
  def part2(): Unit = {
    val intervalList = init.foldLeft( (List[(Long, Long)](), 0L, 0L)) ( (state, p) => {
      val m: Long = if ( p._2 >= state._3) p._2 + 1 else state._3
      if (state._2 < p._1) {
        ( (state._2, p._1 - 1) :: state._1, p._2 + 1, m )
      } else if (state._2 >= p._1 && state._2 <= p._2) {
        ( state._1, p._2 + 1, m )
      } else (state._1, state._2, m)
    })
    val num = intervalList._1.foldLeft(0L)((s, p) => s + (p._2 - p._1 + 1) ) +
      ( if ( intervalList._3 <= max ) max - intervalList._3 + 1 else 0 )
    println(num)
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

}


