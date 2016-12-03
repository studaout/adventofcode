package studaout.y2015

object Day20 {

  def part1(): Unit = {
    val num = 34000000L
    val size = 1000000
    var arr = new Array[Long](size+1)
    for (i <- 1 to size){
      for ( j <- i to size by i) {
        arr(j) += i*10
      }
    }
    println(arr.indexOf(arr.find( _ >= num).getOrElse(-1L)))
  }

  def part2(): Unit = {
    val num = 34000000L
    val size = 1000000
    var arr = new Array[Long](size+1)
    for (i <- 1 to size){
      for ( j <- i to i*50 by i) {
        if ( j <= size ) arr(j) += i*11
      }
    }
    println(arr.indexOf(arr.find( _ >= num).getOrElse(-1L)))
  }

  def main(args: Array[String]) {
    part1()
    part2()
  }

}
