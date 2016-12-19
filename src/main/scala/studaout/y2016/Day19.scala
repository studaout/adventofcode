package studaout.y2016

import studaout.firstLine

object Day19 {

  val inputFile = "/2016/day19-input.txt"
  val init: Int = firstLine(inputFile).toInt

  case class Node(num: Int, var next: Node)

  /**
    * Each Elf brings a present. They all sit in a circle, numbered starting with position 1.
    * Then, starting with the first Elf, they take turns stealing all the presents from the Elf to their left.
    * An Elf with no presents is removed from the circle and does not take turns.
    * For example, with five Elves (numbered 1 to 5):
    *   1
    * 5   2
    *  4 3
    *  Elf 1 takes Elf 2's present.
    *  Elf 2 has no presents and is skipped.
    *  Elf 3 takes Elf 4's present.
    *  Elf 4 has no presents and is also skipped.
    *  Elf 5 takes Elf 1's two presents.
    *  Neither Elf 1 nor Elf 2 have any presents, so both are skipped.
    *  Elf 3 takes Elf 5's three presents.
    *  So, with five Elves, the Elf that sits starting in position 3 gets all the presents.
    *  With the number of Elves given in your puzzle input, which Elf gets all the presents?
    */
  def part1(): Unit = {
    var l = Node(1, null)
    (2 to init).foldLeft(l) ( (c, i) => {
      c.next = Node(i, null)
      c.next
    }).next = l
    while ( l.next != l ) {
      l.next = l.next.next
      l = l.next
    }
    println(l.num)
  }

  /**
    * Realizing the folly of their present-exchange rules, the Elves agree to instead steal presents from the Elf directly
    * across the circle. If two Elves are across the circle, the one on the left (from the perspective of the stealer) is stolen from.
    * The other rules remain unchanged: Elves with no presents are removed from the circle entirely, and the other elves move in
    * slightly to keep the circle evenly spaced.
    * For example, with five Elves (again numbered 1 to 5):
    * The Elves sit in a circle; Elf 1 goes first:
    *       1
    *     5   2
    *      4 3
    *  Elves 3 and 4 are across the circle; Elf 3's present is stolen, being the one to the left.
    *  Elf 3 leaves the circle, and the rest of the Elves move in:
    *       1           1
    *     5   2  -->  5   2
    *      4 -          4
    * Elf 2 steals from the Elf directly across the circle, Elf 5:
    *       1         1
    *     -   2  -->     2
    *       4         4
    *  Next is Elf 4 who, choosing between Elves 1 and 2, steals from Elf 1:
    *
    *      -          2
    *         2  -->
    *      4          4
    *  Finally, Elf 2 steals from Elf 4:
    *      2
    *         -->  2
    *      -
    * So, with five Elves, the Elf that sits starting in position 2 gets all the presents.
    * With the number of Elves given in your puzzle input, which Elf now gets all the presents?
    */
  def part2(): Unit = {
    var l = Node(1, null)
    (2 to init).foldLeft(l) ( (c, i) => {
      c.next = Node(i, null)
      c.next
    }).next = l
    var preDel = l
    for ( i<- 1 until init / 2 ) preDel = preDel.next
    var d = 0
    while ( l.next != l ) {
      preDel.next = preDel.next.next
      d = d + 1
      if ( d == 2 ) {
        preDel = preDel.next
        d = 0
      }
      l = l.next
    }
    println(l.num)

  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }
}
