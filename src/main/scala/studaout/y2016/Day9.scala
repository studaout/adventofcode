package studaout.y2016

import studaout._

object Day9 {

  val inputFile = "/2016/day9-input.txt"

  /**
    * The format compresses a sequence of characters. Whitespace is ignored. To indicate that some sequence should be repeated,
    * a marker is added to the file, like (10x2). To decompress this marker, take the subsequent 10 characters and repeat them 2 times.
    * Then, continue reading the file after the repeated data. The marker itself is not included in the decompressed output.
    *
    * If parentheses or other characters appear within the data referenced by a marker, that's okay - treat it like normal data,
    * not a marker, and then resume looking for markers after the decompressed section.
    *
    * For example:
    * ADVENT contains no markers and decompresses to itself with no changes, resulting in a decompressed length of 6.
    * A(1x5)BC repeats only the B a total of 5 times, becoming ABBBBBC for a decompressed length of 7.
    * (3x3)XYZ becomes XYZXYZXYZ for a decompressed length of 9.
    * A(2x2)BCD(2x2)EFG doubles the BC and EF, becoming ABCBCDEFEFG for a decompressed length of 11.
    * (6x1)(1x3)A simply becomes (1x3)A - the (1x3) looks like a marker, but because it's within a data section of another marker,
    *  it is not treated any differently from the A that comes after it. It has a decompressed length of 6.
    *  (8x2)(3x3)ABCY becomes X(3x3)ABC(3x3)ABCY (for a decompressed length of 18), because the decompressed data from the (8x2) marker
    *  (the (3x3)ABC) is skipped and not processed further.
    *
    *  What is the decompressed length of the file (your puzzle input)? Don't count whitespace.
    */

  def part1(): Unit = {
    val total = lines(inputFile).foldLeft( 0 ) ( (count, line) => {
      val lineRes = line.foldLeft((0,0, Option[String](null))) ( (state, ch) => {
        ch match {
          case c if state._1 > 0 =>
            (state._1-1, state._2, None)
          case '(' if state._3.isEmpty =>
            (0, state._2, Some(""))
          case ')' if state._3.isDefined =>
            val pref = state._3.get
            pref match {
              case r"(\d+)${num}x(\d+)${rep}" =>
                (num.toInt, state._2 + (num.toInt * rep.toInt), None)
              case _ => (0, state._2 + pref.length + 2,None)
            }
          case c if state._3.isDefined =>
            (0,state._2, state._3.map(s => s + c))

          case _ =>
            (0, state._2+1, None)
        }
      })
      count + lineRes._2
    })
    println(total)
  }

  /**
    * In version two, the only difference is that markers within decompressed data are decompressed.
    * This, the documentation explains, provides much more substantial compression capabilities,
    * allowing many-gigabyte files to be stored in only a few kilobytes.
    *
    * For example:
    *  (3x3)XYZ still becomes XYZXYZXYZ, as the decompressed section contains no markers.
    *  X(8x2)(3x3)ABCY becomes XABCABCABCABCABCABCY, because the decompressed data from the (8x2) marker is then further decompressed,
    *  thus triggering the (3x3) marker twice for a total of six ABC sequences.
    *  (27x12)(20x12)(13x14)(7x10)(1x12)A decompresses into a string of A repeated 241920 times.
    *  (25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN becomes 445 characters long.
    *
    *  Unfortunately, the computer you brought probably doesn't have enough memory to actually decompress the file;
    *  you'll have to come up with another way to get its decompressed length.
    *  What is the decompressed length of the file using this improved format?
    */
  def part2(): Unit = {
    var size = 0L
    lines(inputFile).foreach { line =>
      size = size + decompres(line)
    }
    println(size)
  }

  def decompres(line: String): Long = {
    var size = 0L
    var i = 0
    while (i < line.length) {
      val ch = line.charAt(i)
      ch match {
        case '(' =>
          var pref = ""
          var j = i + 1
          while (line.charAt(j) != ')') {
            pref = pref + line.charAt(j)
            j += 1
          }
          pref match {
            case r"(\d+)${num}x(\d+)${rep}" =>
              size = size + (decompres(line.substring(j+1, j + 1 + num.toInt)) * rep.toInt)
              i = j + num.toInt + 1
            case _ => size = size + pref.length + 2
          }
        case _ =>
          size = size + 1
          i = i + 1
      }
    }
    size
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

}
