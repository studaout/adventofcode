package studaout.y2015

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random
import studaout._

object Day19 {

  def part1(): Unit ={
    val inputFile = "/2015/day19-input.txt"
    val lns = lines(inputFile).toArray
    val str = lns.last
    var maxsize = 0
    val map = new mutable.HashMap[String, List[String]]()
    for( i <- 0 until lns.length-2 ) {
      val line = lns(i)
      line match {
        case r"([a-zA-Z]+)${key} => ([a-zA-Z]+)${value}" =>
          val l = map.getOrElse(key, List[String]())
          if ( key.length > maxsize ) maxsize = key.length
          map.put(key, value :: l)
        case default => throw new Exception("unknown: " + default )
      }
    }
    var count = 0L
    val set = mutable.HashSet[String]()
    for ( i <- 0 until str.length ){
      val ms = if ( i <= str.length-maxsize ) maxsize else str.length-i
      for ( j <- 1 to ms ) {
        val k = str.substring(i,i+j)
        map.getOrElse(k, List()).foreach{ kv=>
          set += str.substring(0,i) + kv + str.substring(i+j, str.length)
        }
      }
    }
    println(set.size)
  }

  def part2(): Unit ={
    val inputFile = "/2015/day19-input.txt"
    val lns = lines(inputFile).toArray
    val str = lns.last
    val list = new ListBuffer[(String,String)]()
    for( i <- 0 until lns.length-2 ) {
      val line = lns(i)
      line match {
        case r"([a-zA-Z]+)${key} => ([a-zA-Z]+)${value}" =>
          val p = (key,value)
          list+=p
        case default => throw new Exception("unknown: " + default )
      }
    }
    var arr = list.toArray
    var count = 0L
    var target = str
    while ( target != "e" ) {
      var tmp = target
      for ( p <- arr) {
        if ( tmp.contains(p._2) ) {
          tmp = tmp.replaceFirst(p._2, p._1)
          count += 1
        }
      }
      if ( target == tmp) {
        println("shuffling")
        count = 0L
        arr = Random.shuffle(arr.toList).toArray
        target = str
      } else {
        target = tmp
      }
    }
    println(count)
  }


  def main(args: Array[String]) {
    part1()
    part2()
  }

}
