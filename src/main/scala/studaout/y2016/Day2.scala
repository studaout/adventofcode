package studaout.y2016

import scala.io.Source

object Day2 {

  def part1(arr: Array[String]): Unit = {
    val a = Array(Array('1','2','3'), Array('4','5','6'), Array('7','8','9'))
    val code = arr.foldLeft(("", 1, 1))( ( pos, str ) => {
      val linePos = str.foldLeft( (pos._2, pos._3))( (p, c) => {
        c match {
          case 'L' =>
            if (p._1 > 0) (p._1-1, p._2) else (p._1, p._2)
          case 'R' =>
            if (p._1 < 2) (p._1+1, p._2) else (p._1, p._2)
          case 'U' =>
            if (p._2 > 0) (p._1, p._2-1) else (p._1, p._2)
          case _ => //D
            if (p._2 < 2) (p._1, p._2+1) else (p._1, p._2)
        }
      })
      (pos._1+a(linePos._2)(linePos._1), linePos._1, linePos._2)
    })
    println(code._1)
  }

  def part2(arr: Array[String]): Unit = {
    val a = Array(
      Array('x','x','1','x','x'),
      Array('x','2','3','4','x'),
      Array('5','6','7','8','9'),
      Array('x','A','B','C','x'),
      Array('x','x','D','x','x')
    )
    val code = arr.foldLeft(("", 1, 1))( ( pos, str ) => {
      val linePos = str.foldLeft( (pos._2, pos._3))( (p, c) => {
        val move = c match {
          case 'L' =>
            if (p._1 > 0) (p._1-1, p._2) else (p._1, p._2)
          case 'R' =>
            if (p._1 < 4) (p._1+1, p._2) else (p._1, p._2)
          case 'U' =>
            if (p._2 > 0) (p._1, p._2-1) else (p._1, p._2)
          case _ => //D
            if (p._2 < 4) (p._1, p._2+1) else (p._1, p._2)
        }
        if (  a(move._2)(move._1) == 'x' ) p else move
      })
      (pos._1+a(linePos._2)(linePos._1), linePos._1, linePos._2)
    })
    println(code._1)
  }

  def main(args: Array[String]): Unit = {
    val path = "/2016/day2-input.txt"
    val lines = Source.fromInputStream(getClass.getResourceAsStream(path)).getLines().toArray//.foldLeft("")( (p,s) => p + s)
    part1(lines)
    part2(lines)
  }
}

