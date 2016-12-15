package studaout.y2016

import studaout._

object Day15 {

  val inputFile = "/2016/day15-input.txt"

    /**
    * Furthermore, the discs are spaced out so that after you push the button, one second elapses
    * before the first disc is reached, and one second elapses as the capsule passes from one disk to the one below it.
    * So, if you push the button at time=100, then the capsule reaches the top disc at time=101, the second disc at time=102,
    * the third disc at time=103, and so on.
    * The button will only drop a capsule at an integer time - no fractional seconds allowed.
    * For example, at time=0, suppose you see the following arrangement:
    * Disc #1 has 5 positions; at time=0, it is at position 4.
    * Disc #2 has 2 positions; at time=0, it is at position 1.
    * If you press the button exactly at time=0, the capsule would start to fall; it would reach the first disc at time=1.
    * Since the first disc was at position 4 at time=0, by time=1 it has ticked one position forward. As a five-position disc,
    * the next position is 0, and the capsule falls through the slot.
    * Then, at time=2, the capsule reaches the second disc. The second disc has ticked forward two positions at this point: it started at position 1,
    * then continued to position 0, and finally ended up at position 1 again. Because there's only a slot at position 0, the capsule bounces away.
    * If, however, you wait until time=5 to push the button, then when the capsule reaches each disc,
    * the first disc will have ticked forward 5+1 = 6 times (to position 0),
    * and the second disc will have ticked forward 5+2 = 7 times (also to position 0).
    * In this case, the capsule would fall through the discs and come out of the machine.
    * However, your situation has more than two discs; you've noted their positions in your puzzle input.
    * What is the first time you can press the button to get a capsule?
    */

  case class Disk(num: Int, posNum: Int, init: Int)

  val disks: List[Disk] = {
    lines(inputFile).map {
      case r"Disc #(\d)${diskNum} has (\d+)${posNum} positions; at time=0, it is at position (\d+)${initPos}." =>
        Disk(diskNum.toInt, posNum.toInt, initPos.toInt)
    }.toList

  }

  def find(i:Int, disks: List[Disk]) : Int = {
    if ( ! disks.exists{ d => (i + d.num + d.init)%d.posNum != 0  } ) i else find(i+1, disks)
  }

  def part1(): Unit = {
    println(find(0, disks))
  }

  //but a new disc with 11 positions and starting at position 0 has appeared exactly one second below the previously-bottom disc.
  def part2(): Unit = {
    println(find(0, disks ::: List(Disk(7,11,0))))
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

}
