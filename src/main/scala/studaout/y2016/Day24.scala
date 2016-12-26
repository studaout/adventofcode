package studaout.y2016

import studaout._

object Day24 {

  val inputFile = "/2016/day24-input.txt"
  case class Point(x: Int, y: Int, v: Int)
  case class Path(p1: Point, p2: Point, l: Int)

  val mat: Array[Array[Int]] = lines(inputFile).map { line =>
    line.map {
      case '.' => -1
      case '#' => -2
      case ch if ch >= '0' && ch <= '9' => ch.toInt
    }.toList.toArray
  }.toArray

  val my: Int = mat.length
  val mx: Int = mat(0).length

  val points: Array[Point] = mat.zipWithIndex.foldLeft( List[Point]() )((l,arr) => {
    l ::: arr._1.zipWithIndex
      .foldLeft(List[Point]()) ( (lr, i) => if ( i._1 > -1 ) Point(i._2, arr._2, i._1) :: lr else lr )
      .reverse
  }).sortBy(_.v).toArray

  mat.foreach( arr => arr.zipWithIndex.foreach { i => if ( i._1 > -1 ) arr(i._2) = -1} )

  val paths: Array[Array[Int]] = new Array(points.length)
  for ( i<- paths.indices ) paths(i) = new Array(points.length)
  for ( i <- points.indices ) {
    for ( j<- points.indices ) {
      paths(j)(i) = if ( i == j ) 0 else findPath(points(i), points(j)).l
    }
  }

  def findPath(p1: Point, p2: Point): Path = {
    val m = mat.map( a => a.toList.toArray )
    var found = false
    var cur: List[(Int, Int)] = List((p1.x,p1.y))
    m(p1.y)(p1.x) = 1
    while( ! found ) {
      cur = cur.flatMap{ c =>
        if ( c == (p2.x,p2.y) ) {
          found = true
          List()
        } else {
          val v = m(c._2)(c._1) + 1
          List((c._1 - 1, c._2), (c._1 + 1, c._2), (c._1, c._2 - 1), (c._1, c._2 + 1))
            .filter { p =>
              if (p._2 >= 0 && p._2 < my && p._1 >= 0 && p._1 < mx && m(p._2)(p._1) == -1 ) {
                m(p._2)(p._1) = v; true
              } else false
            }
        }
      }
    }
    var cnt = 0
    var c = (p2.x,p2.y)
    while ( found ) {
      val v = m(c._2)(c._1)
      c = List( (c._1-1, c._2), (c._1+1, c._2), (c._1, c._2-1), (c._1, c._2+1)).find { p =>
        p._1 >= 0 && p._1 < mx && p._2 >= 0 && p._2 < my && m(p._2)(p._1) < v && m(p._2)(p._1) > -1
      }.get
      cnt = cnt + 1
      if ( c == (p1.x, p1.y) ) found = false
    }
    Path(p1,p2, cnt)
  }

  def findShortestPath(cl: Int, cp: List[Int]) : Int = {
    val rest = points.indices.filter(i => ! cp.contains(i) )
    if ( rest.isEmpty ) cl else {
      val last = cp.head
      rest.map { i =>
        val p = paths(last)(i) + cl
        findShortestPath(p, i :: cp)
      }.foldLeft(Int.MaxValue) ( (m, r) => Math.min(m, r))
    }
  }

  def findShortestPathWithBack(cl: Int, cp: List[Int]) : Int = {
    val rest = points.indices.filter(i => ! cp.contains(i) )
    if ( rest.isEmpty ) {
      cl + paths(cp.head)(0)
    }else {
      val last = cp.head
      rest.map { i =>
        val p = paths(last)(i) + cl
        findShortestPathWithBack(p, i :: cp)
      }.foldLeft(Int.MaxValue) ( (m, r) => Math.min(m, r))
    }
  }

  /**
    * You've finally met your match; the doors that provide access to the roof are locked tight, and all of the controls
    * and related electronics are inaccessible. You simply can't reach them.
    * The robot that cleans the air ducts, however, can.
    * It's not a very fast little robot, but you reconfigure it to be able to interface with some of the exposed wires that
    * have been routed through the HVAC system. If you can direct it to each of those locations, you should be able to bypass the security controls.
    * You extract the duct layout for this area from some blueprints you acquired and create a map with the relevant locations
    *  marked (your puzzle input). 0 is your current location, from which the cleaning robot embarks; the other numbers are (in no particular order)
    *  the locations the robot needs to visit at least once each. Walls are marked as #, and open passages are marked as .. Numbers behave like open passages.
    *  For example, suppose you have a map like the following:
    *  ###########
    *  #0.1.....2#
    *  #.#######.#
    *  #4.......3#
    *  ###########
    *  To reach all of the points of interest as quickly as possible, you would have the robot take the following path:
    *   0 to 4 (2 steps)
    *   4 to 1 (4 steps; it can't move diagonally)
    *   1 to 2 (6 steps)
    *   2 to 3 (2 steps)
    *   Since the robot isn't very fast, you need to find it the shortest route. This path is the fewest steps (in the above example, a total of 14)
    *   required to start at 0 and then visit every other location at least once.
    *   Given your actual map, and starting from location 0, what is the fewest number of steps required to visit every non-0 number marked
    *   on the map at least once?
    */
  def part1(): Unit = {
    println(findShortestPath(0, List(0)))
  }

  /**
    * Of course, if you leave the cleaning robot somewhere weird, someone is bound to notice.
    * What is the fewest number of steps required to start at 0, visit every non-0 number marked on the map at least once, and then return to 0?
    */
  def part2(): Unit = {
    println(findShortestPathWithBack(0, List(0)))
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

}
