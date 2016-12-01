package studaout.y2016

import studaout._

case class Point(x:Int, y:Int) {
  def +(p: Point): Point = Point(this.x + p.x, this.y + p.y)
}

object  Day1 {

  def part1(arr: Array[String]): Unit = {
    val last = arr.foldLeft((Point(0,0), 0))((p, s) => {
      val d = getDirection(s, p._2)
      (p._1+d._1,d._2)
    })
    val r = math.abs(0 - last._1.x) + math.abs(0 - last._1.y)
    println(r)
  }

  def part2(arr :Array[String] ) : Unit = {
    def find(set: Set[Point], cur: Point, dir: Int, i: Int) : Point = {
      if ( i == arr.length ) cur else {
        val dirList = getDirectionList(arr(i), dir)
        val nextStep = dirList._1.foldLeft( (cur, set, Option.empty[Point]) )( (cp, step) => {
          val np = cp._1 + step
          if ( cp._3.isEmpty && cp._2.contains( np) ) (np, cp._2 + np, Option(np)) else (np, cp._2 + np, cp._3)
        })
        if ( nextStep._3.isDefined ) nextStep._3.get else find(nextStep._2, nextStep._1, dirList._2, i+1)
      }
    }
    val last = find(Set(), Point(0,0), 0, 0)
    val r = math.abs(0 - last.x) + math.abs(0 - last.y)
    println(r)
  }

  def main(args: Array[String]): Unit = {
    val inputFile = "/2016/day1-input.txt"
    val line = firstLine(inputFile)
    val arr = line.split(", ")
    part1(arr)
    part2(arr)
  }

  def getDirection(str: String, curDir: Int): (Point,Int) = {
    var steps = str.substring(1).toInt
    val d = if ( str.startsWith("R")) {
      if ( curDir < 3) curDir+1 else 0
    } else {
      if ( curDir > 0) curDir-1 else 3
    }
    d match {
      case 0 => (Point(0, steps), d)
      case 1 => (Point(-steps, 0), d)
      case 2 => (Point(0, -steps), d)
      case _ => (Point(steps, 0), d)
    }
  }

  def getDirectionList(str: String, curDir: Int): (List[Point],Int) = {
    var steps = str.substring(1).toInt
    val d = if ( str.startsWith("R")) {
      if ( curDir < 3) curDir+1 else 0
    } else {
      if ( curDir > 0) curDir-1 else 3
    }
    d match {
      case 0 => ((1 to steps).foldLeft(List[Point]())((l,i) => Point(0,1) :: l ), d)
      case 1 => ((1 to steps).foldLeft(List[Point]())((l,i) => Point(-1,0) :: l ), d)
      case 2 => ((1 to steps).foldLeft(List[Point]())((l,i) => Point(0,-1) :: l ), d)
      case _ => ((1 to steps).foldLeft(List[Point]())((l,i) => Point(1,0) :: l ), d)
    }
  }

}
