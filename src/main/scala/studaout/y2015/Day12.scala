package studaout.y2015

import studaout._

object Day12 {

  val inputFile = "/2015/day12-input.txt"

  def part1(): Unit = {
    var sum = 0L
    for (line <- lines(inputFile)) {
      val l1 = line.replaceAll("\".*\"", " ")
      val l2 = line.replaceAll("[^0-9-]", " ")
      l2.split(" ").foreach(s => {
        val t = s.trim
        if (!t.isEmpty) {
          println(t)
          sum += t.toLong
        }
      })
      println("-------------------")
    }
    println("sum: " + sum)
  }

  def part2_1(): Unit ={
    var sum = 0L
    val line = firstLine(inputFile)
    var found = true
    var str = line
    while ( found ) {
      val i = str.indexOf(":\"red\"")
      if ( i >=0 ) {
        var l = i-1
        var moved = true
        var count = 0
        while ( l > 0 && moved ) {
          val c = str.charAt(l)
          if ( c == '{' && count == 0) moved = false else {
            if ( c == '{' ) count -= 1
            if ( c == '}' ) count += 1
            l -=1
          }
        }
         moved = true
        count = 0
        var r = i + 6
        while ( r < str.length && moved ) {
          val c = str.charAt(r)
          if ( c == '}' && count == 0) moved = false else {
            if ( c == '}' ) count -= 1
            if ( c == '{' ) count += 1
            r +=1
          }
        }
        str = str.substring(0, l) + str.substring(r+1, str.length)
      } else found = false
    }
    val l1 = str.replaceAll("\".*\""," ")
    val l2 = str.replaceAll("[^0-9-]", " ")
    l2.split(" ").foreach(s => {
      val t = s.trim
      if (!t.isEmpty) {
        println(t)
        sum += t.toLong
      }
    })
    println("sum: " + sum)

  }
//
//  def part2(): Unit ={
//    var sum = 0L
//    val line = firstLine(inputFile)
//    val jsonAst = line.parseJson
//    println(jsonAst.prettyPrint)
//    //jsonAst.asJsObject.fields.
//  }
//
  def main(args: Array[String]) {
    part1() //111754
    part2_1()
  }

}
