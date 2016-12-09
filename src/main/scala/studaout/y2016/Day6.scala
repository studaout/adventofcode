package studaout.y2016

import studaout._

/**
  * Something is jamming your communications with Santa. Fortunately, your signal is only partially jammed, and protocol in situations
  * like this is to switch to a simple repetition code to get the message through.
  * In this model, the same message is sent repeatedly. You've recorded the repeating message signal (your puzzle input),
  * but the data seems quite corrupted - almost too badly to recover. Almost.
  * All you need to do is figure out which character is most frequent for each position.
  *
  * In this modified code, the sender instead transmits what looks like random data, but for each character,
  * the character they actually want to send is slightly less likely than the others. Even after signal-jamming noise,
  * you can look at the letter distributions in each column and choose the least common letter to reconstruct the original message.
  */

object Day6 {

  val inputFile = "/2016/day6-input.txt"

  def part1(): Unit = {
    val init = (0 to 7).map(_ => Map[Char, Int]()).toArray
    val m = lines(inputFile).foldLeft(init) ( (arr, line) => {
      line.zipWithIndex.map { p =>
        val map = arr(p._2)
        val cnt = map.getOrElse(p._1, 0) + 1
        map + (p._1 -> cnt)
      }.toArray
    } )
    m.foreach{ map => print(map.toList.sortBy( _._2).reverse.head._1) }
    println()
  }

  def part2(): Unit = {
    val init = (0 to 7).map(_ => Map[Char, Int]()).toArray
    val m = lines(inputFile).foldLeft(init) ( (arr, line) => {
      line.zipWithIndex.map { p =>
        val map = arr(p._2)
        val cnt = map.getOrElse(p._1, 0) + 1
        map + (p._1 -> cnt)
      }.toArray
    } )
    m.foreach{ map => print(map.toList.sortBy( _._2).head._1) }
    println()
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

}
