package studaout.y2015

import studaout._

object Day16 {

  case class Sue(name: Int, properties: Map[String, Int])

  val rprop = "([a-z]+): ([0-9]+)".r


//  children: 3
//  cats: 7
//  samoyeds: 2
//  pomeranians: 3
//  akitas: 0
//  vizslas: 0
//  goldfish: 5
//  trees: 3
//  cars: 2
//  perfumes: 1

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  def cmp(sue1: Sue, sue2: Sue) : Boolean = {
    val keys = sue1.properties.keySet ++ sue2.properties.keySet
    keys.map {k => (sue1.properties.getOrElse(k, -1), sue2.properties.getOrElse(k, -1), k)}
      .count(p =>
        p._1 == -1 || p._2 == -1 || p._1 == p._2
      ) == keys.size
  }

  def cmp(sue1: Sue, sue2: Sue, ops: Map[String, (Int,Int)=>Boolean]) : Boolean = {
    val keys = sue1.properties.keySet ++ sue2.properties.keySet
    keys.map {k =>
      (k,
        sue1.properties.getOrElse(k, -1),
        sue2.properties.getOrElse(k, -1), ops.getOrElse(k, (a:Int,b:Int)=> a == b))}
      .count(p =>
        p._2 == -1 || p._3 == -1 || p._4(p._2, p._3)
      ) == keys.size
  }

  def part1_2(): Unit = {
    //Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5
    val inputFile = "/2015/day16-input.txt"
    val sueList = lines(inputFile).map {
      case r"Sue (\d+)${name}: ([a-z0-9:, ]+)${props}" => (name.toInt, props)
      case default => throw new Exception("unknown: " + default)
    }.map { p =>
      Sue(
        p._1,
        rprop.findAllMatchIn(p._2).map(m => m match {
          case rprop(pn, pv) => (pn, pv.toInt)
        }).toMap
      )
    }.toList

    val MFCSAM = Map[String, Int](
      "children" -> 3, "cats" -> 7, "samoyeds" -> 2,
      "pomeranians" -> 3, "akitas" -> 0, "vizslas" -> 0,
      "goldfish" -> 5, "trees" -> 3, "cars" -> 2,
      "perfumes" -> 1
    )
    val mfscamSue = Sue(-1, MFCSAM)
    val sc1 = sueList.find(s => cmp(s, mfscamSue)).get.name
    printf("num1: %d \n", sc1)
    val gt = (a: Int, b: Int) => a > b
    val lt = (a: Int, b: Int) => a < b
    val ops = Map[String, (Int,Int)=> Boolean](
    "cats"-> gt, "trees" -> gt, "pomeranians" -> lt, "goldfish" -> lt
    )
    val sc2 = sueList.find( s=> cmp(s, mfscamSue, ops)).get.name
    printf("num2: %d \n", sc2)
  }

  def main(args: Array[String]) {
    part1_2()
  }

}
