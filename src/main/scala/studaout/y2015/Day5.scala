package studaout.y2015

import studaout._


object Day5 {

  val inputFile = "/2015/day5-input.txt"

  def part1(): Unit = {
    val vowels = "aeiou"
    var niceCount = 0
    for ( line <- lines(inputFile) ) {
      var doubles = 0
      var depricated = 0
      var vowel = if ( vowels.contains(line.charAt(0)) ) 1 else 0
      for ( i <- 1 until line.length ) {
        val c = line.charAt(i)
        if ( vowels.contains(c) ) vowel += 1
        val prev = line.charAt(i-1)
        if ( c == prev ) doubles += 1
        c match {
          case 'b' => if ( prev == 'a' ) depricated +=1
          case 'd' => if ( prev == 'c' ) depricated +=1
          case 'q' => if ( prev == 'p' ) depricated +=1
          case 'y' => if ( prev == 'x' ) depricated +=1
          case _ =>
        }
      }
      if ( doubles > 0 && vowel > 2 && depricated == 0 ) niceCount += 1
      //It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
      //It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
      //It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
    }
    println("nice count: " + niceCount)
  }

  def part2(): Unit = {
    var niceCount = 0
    for ( line <- lines(inputFile) ) {
      var doubles = 0
      var repeat = 0
      val cache = collection.mutable.HashMap[String, Int]()
      if ( line.length > 2 ) {
        cache.put(line.substring(0,2), 0)
        for (i <- 2 until line.length) {
          val c = line.charAt(i)
          val pp = line.charAt(i - 2)
          if (c == pp) doubles += 1
          val k = line.substring(i-1, i+1)
          val v = cache.getOrElse(k, -1)
          if ( v >= 0 ) {
            if ( v < i-2 ) repeat += 1
          } else {
            cache.put(k, i-1)
          }
        }
      }
      if ( doubles > 0 && repeat > 0 ) niceCount += 1
    }
    println("nice count: " + niceCount)
//    It contains a pair of any two letters that appears at least twice in the string without overlapping,
//    like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
//      It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.
  }

  def main(args: Array[String]) {
    part1()
    part2()
  }
}
