package studaout.y2015

import studaout._

object Day14 {

  val inputFile = "/2015/day14-input.txt"

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  case class Deer(name:String, speed:Int, speedTime:Int, rest:Int) {
    val cycleTime = speedTime + rest
  }
  case class Pos(deer:Deer, curTime:Int, dist:Int, score:Int)

  def part1(): Unit = {
    val time = 2503
    val max = lines(inputFile).map {
      //Dancer can fly 27 km/s for 5 seconds, but then must rest for 132 seconds.
      case r"([a-zA-Z]+)${name} can fly (\d+)${speed} km/s for (\d+)${speedTime} seconds, but then must rest for (\d+)${rest} seconds\." =>
        Deer(name, speed.toInt, speedTime.toInt, rest.toInt)
      case default => throw new Exception("unknown: " + default )
    }.map { d =>
      val speedDist = d.speedTime * d.speed
      val cycles = time/d.cycleTime
      val restTime = time - (cycles * d.cycleTime)
      if ( restTime >= d.speedTime) speedDist*(cycles+1)  else (d.speedTime - restTime)*d.speed + speedDist*cycles
    }.max
    printf("max distance: %d\n", max)
  }

  def part2(): Unit ={
    val time = 2503

    val arr = lines(inputFile).map {
      //Dancer can fly 27 km/s for 5 seconds, but then must rest for 132 seconds.
      case r"([a-zA-Z]+)${name} can fly (\d+)${speed} km/s for (\d+)${speedTime} seconds, but then must rest for (\d+)${rest} seconds\." =>
        Deer(name, speed.toInt, speedTime.toInt, rest.toInt)
      case default => throw new Exception("unknown: " + default )
    }.map { d=> Pos(d, 0, 0, 0)}.toArray

    for ( t <- 1 to time ) {
      for ( i <- arr.indices ) {
        val p = arr(i)
        //Pos(deer:Deer, curTime:Int, dist:Int, score:Int)
        val ct = if (p.curTime < p.deer.cycleTime) p.curTime +1 else 1
        var dist = if ( ct <= p.deer.speedTime ) p.dist + p.deer.speed else p.dist
        arr(i) = Pos(p.deer, ct, dist, p.score)
      }
      val m = arr.maxBy( p => p.dist)
      for ( i <- arr.indices ) {
        val p = arr(i)
        if ( p.dist == m.dist )arr(i) = Pos(p.deer, p.curTime, p.dist, p.score+1)
      }
    }
    val m = arr.maxBy(p=>p.score)

    printf("max score: %d\n", m.score)
  }

  def main(args: Array[String]) {
    part1()
    part2()
  }


}
