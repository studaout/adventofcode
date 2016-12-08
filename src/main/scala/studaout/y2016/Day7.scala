package studaout.y2016

import studaout._

object Day7 {

  val inputFile = "/2016/day7-input.txt"

  /*
  An IP supports TLS if it has an Autonomous Bridge Bypass Annotation, or ABBA. An ABBA is any four-character
  sequence which consists of a pair of two different characters followed by the reverse of that pair,
  such as xyyx or abba. However, the IP also must not have an ABBA within any hypernet sequences,
  which are contained by square brackets.
   */
  def part1(): Unit = {
    def res = lines(inputFile).foldLeft(0) ((i, line) => {

      def f(i: Int, str: String, isIn : Boolean, cnt: Int) : Int = {
        if ( i >= line.length ) { if (cnt > 0) 1 else 0 } else {
          line.charAt(i) match {
            case '[' => f(i+1, "", isIn = true, cnt)
            case ']' => f(i+1, "", isIn = false, cnt)
            case ch =>
              val s = str + ch
              val l = s.length
              if ( l > 3 ) {
                if (s.charAt(l - 1) == s.charAt(l - 4) && s.charAt(l - 2) == s.charAt(l - 3) && s.charAt(l - 1) != s.charAt(l - 2)) {
                  if (isIn) 0 else f(i+1, s, isIn, cnt+1)
                } else f(i+1, s, isIn, cnt)
              } else f(i+1, s, isIn, cnt)
          }
        }
      }
      i + f(0, "", isIn = false, 0)
    })
    println(res)
  }

  /*
  An IP supports SSL if it has an Area-Broadcast Accessor, or ABA, anywhere in the supernet sequences (outside any
  square bracketed sections), and a corresponding Byte Allocation Block, or BAB, anywhere in the hypernet sequences.
   An ABA is any three-character sequence which consists of the same character twice with a different character
   between them, such as xyx or aba. A corresponding BAB is the same characters but in reversed positions: yxy and bab,
   respectively.
   */

  def part2(): Unit = {
    def res = lines(inputFile).foldLeft(0) ((i, line) => {
      def f(i: Int, str: String, isIn : Boolean, inMap : Map[String, String], outMap : Map[String, String])
      : (Map[String, String], Map[String, String]) = {
        if ( i >= line.length ) (inMap, outMap) else {
          line.charAt(i) match {
            case '[' => f(i+1, "", isIn = true, inMap, outMap)
            case ']' => f(i+1, "", isIn = false, inMap, outMap)
            case ch =>
              val s = str + ch
              val l = s.length
              if (l > 2) {
                if (s.charAt(l - 1) == s.charAt(l - 3) && s.charAt(l - 1) != s.charAt(l - 2)) {
                  val matchStr = s.substring(l-3)
                  if ( isIn ) {
                    f(i+1, s, isIn, inMap + (matchStr-> matchStr), outMap)
                  } else {
                    f(i+1, s, isIn, inMap, outMap + (matchStr-> matchStr))
                  }
                } else f(i+1, s, isIn, inMap, outMap)
              } else f(i+1, s, isIn, inMap, outMap)
          }
        }
      }
      val maps = f(0,"", isIn = false, Map(), Map())
      val res = maps._2.find ({ p =>
        val s = "" + p._1.charAt(1) + p._1.charAt(0) + p._1.charAt(1)
        maps._1.contains(s)
      })
      if ( res.isDefined ) i + 1 else i
    })
    println(res)
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }
}
