package studaout.y2015

import studaout._

object Day8 {

  val inputFile = "/2015/day8-input.txt"

  def part1(): Unit = {
    val res = lines(inputFile).foldLeft( (0,0)) ((state, line) => {
      val trimmeed = line.trim()
      (state._1 + line.length, state._2 + parse1(trimmeed.substring(1, trimmeed.length-1)))
    })
    println("part1: tot=" + res._1 + " inMemory=" + res._2 + "  res=" +(res._1-res._2))
  }

  def part2(): Unit = {
    val res = lines(inputFile).foldLeft( (0,0)) ((state, line) => {
      val trimmeed = line.trim()
      (state._1 + line.length, state._2 + (6 + parse2(trimmeed.substring(1, trimmeed.length-1))))
    })
    println("part1: tot=" + res._1 + " inMemory=" + res._2 + "  res=" +(res._1-res._2))
  }

  def parse1(str:String): Int = {
    val r1 = str.replace("\\\\", " ")
    val r2 = r1.replace("\\\"", " ")
    val r3 = r2.replaceAll("\\\\x[a-f0-9]{2}", " ")
    //println(str + "---" + r1 + "---" + r2 + "---" + r3)
    r3.length
  }

  def parse2(str:String): Int = {
    val r1 = str.replace("\\\\", "    ")
    val r2 = r1.replace("\\\"", "    ")
    val r3 = r2.replaceAll("\\\\x[a-f0-9]{2}", "     ")
    //println(str + "---" + r1 + "---" + r2 + "---" + r3)
    r3.length
  }

  def main(args: Array[String]) {
    part1()
    part2()
  }
}
