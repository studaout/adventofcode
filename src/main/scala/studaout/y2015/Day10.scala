package studaout.y2015

/**
 * Today, the Elves are playing a game called look-and-say.
 * They take turns making sequences by reading aloud the previous sequence and using that reading as the next sequence.
 * For example, 211 is read as "one two, two ones", which becomes 1221 (1 2, 2 1s).
 */
object Day10 {

  def part1(): Unit ={
    var in = "1113222113"
    for ( i <- 0 until 40 ) {
      var counter = 0
      var p = in.charAt(0)
      val buf = new StringBuilder()
      for ( c <- in ) {
        if ( p == c ) counter += 1 else {
          buf.append(counter).append(p)
          p = c
          counter = 1
        }
      }
      buf.append(counter).append(p)
      in = buf.toString()
    }
    //println(in)
    println(in.length)
  }

  def part2(): Unit = {
    var in = "1113222113"
    for ( i <- 0 until 50 ) {
      var counter = 0
      var p = in.charAt(0)
      val buf = new StringBuilder()
      for ( c <- in ) {
        if ( p == c ) counter += 1 else {
          buf.append(counter).append(p)
          p = c
          counter = 1
        }
      }
      buf.append(counter).append(p)
      in = buf.toString()
    }
    //println(in)
    println(in.length)
  }

  def main(args: Array[String]) {
    part1()
    part2()
  }
}
