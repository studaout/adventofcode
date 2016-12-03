package studaout.y2015

import scala.collection.immutable.HashMap
import studaout._

object Day13 {

  val inputFile = "/2015/day13-input.txt"

  class Person(val name: String, val map: HashMap[String, Int]) {
    def this(name:String) {
      this(name, new HashMap[String, Int]())
    }
    def get(name:String) : Int = map.getOrElse(name, 0)
    def put(n:String, rate:Int) : Person = new Person(name, map.+((n,rate)))
    def put(p: Person) : Person = new Person(name, this.map.merged(p.map)(null))
  }

  case class MaxOp() extends Function1[Array[Person], Long] {
    private var max = Long.MinValue

    def apply(arr: Array[Person]): Long ={
      var currMax = 0
      for ( i <- 1 until arr.length ) {
        val p = arr(i-1)
        val c = arr(i)
        currMax += ( p.get(c.name) + c.get(p.name))
      }
      val p = arr(0)
      val c = arr(arr.length-1)
      currMax += ( p.get(c.name) + c.get(p.name))
      if ( currMax > max ) max = currMax
      max
    }
    def get = max
  }

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

//  def part1(): Unit = {
//    val inputFile = "src/main/resources/day13-input.txt"
//    val map = new mutable.HashMap[String, Person]()
//    for (line <- Source.fromFile(inputFile).getLines()) {
//      line match {
//        case r"([a-zA-Z]+)${name1} would (lose|gain)${action} (\d+)${amount} happiness units by sitting next to ([a-zA-Z]+)${name2}\." =>
//          val person = map.getOrElse(name1, Person(name1))
//          val rate = if ( action == "lose" ) -1 * amount.toInt else amount.toInt
//          person.put(name2, rate)
//          map.put(name1, person)
//          //printf("%s %s %s %s \n", name1, action, amount, name2)
//        case _ =>
//      }
//    }
//    val arr = map.values.toArray
//    val maxOp = MaxOp()
//    permutations(0, arr, maxOp)
//    printf("best: %d \n", maxOp.get)
//  }

  def permutations(start: Int, arr: Array[Person], f: Function1[Array[Person], Long]): Unit = {
    val n = arr.length
    if ( start == n-1 ) {
      f(arr)
    } else {
      for ( i <- start until n ) {
        swap(start, i, arr)
        permutations(start+1, arr, f)
        swap(start, i, arr)
      }
    }
  }

  def swap(i:Int, j:Int, arr: Array[Person]): Unit = {
    val p = arr(i)
    arr(i) = arr(j)
    arr(j) = p
  }

//  def part2(): Unit = {
//    val inputFile = "src/main/resources/day13-input.txt"
//    val map = new mutable.HashMap[String, Person]()
//    for (line <- Source.fromFile(inputFile).getLines()) {
//      line match {
//        case r"([a-zA-Z]+)${name1} would (lose|gain)${action} (\d+)${amount} happiness units by sitting next to ([a-zA-Z]+)${name2}\." =>
//          val person = map.getOrElse(name1, Person(name1))
//          val rate = if ( action == "lose" ) -1 * amount.toInt else amount.toInt
//          person.put(name2, rate)
//          map.put(name1, person)
//        //printf("%s %s %s %s \n", name1, action, amount, name2)
//        case _ =>
//      }
//    }
//    val me = Person("me")
//    map.keySet.foreach(p => me.put(p, 0))
//    map.values.foreach( p => p.put("me", 0))
//    map.put("me", me)
//    val arr = map.values.toArray
//    val maxOp = MaxOp()
//    permutations(0, arr, maxOp)
//    printf("best: %d \n", maxOp.get)
//  }

  def part1_1(): Unit = {
    val map = lines(inputFile).map {
      case r"([a-zA-Z]+)${name1} would (lose|gain)${action} (\d+)${amount} happiness units by sitting next to ([a-zA-Z]+)${name2}\." =>
        val person = new Person(name1)
        val rate = if (action == "lose") -1 * amount.toInt else amount.toInt
        person.put(name2, rate)
      case _ => throw new Exception("unknown" )
    }.toList.groupBy(_.name).map { case (name, list) =>
      (name, list.reduce((p1,p2) => p2.put(p1)))
    }
    val arr = map.values.toArray
    val maxOp = MaxOp()
    permutations(0, arr, maxOp)
    printf("best: %d \n", maxOp.get)
  }

  def part2_2(): Unit = {
    val map = lines(inputFile).map {
      case r"([a-zA-Z]+)${name1} would (lose|gain)${action} (\d+)${amount} happiness units by sitting next to ([a-zA-Z]+)${name2}\." =>
        val person = new Person(name1)
        val rate = if (action == "lose") -1 * amount.toInt else amount.toInt
        person.put(name2, rate)
      case _ => throw new Exception("unknown" )
    }.toList.groupBy(_.name).map { case (name, list) =>
      (name, list.reduce((p1,p2) => p1.put(p2)))
    }
    val me = map.keySet.foldLeft(new Person("me"))( (p,s) => p.put(s,0))
    val meMap = map.map { case (name, person) => (name, person.put("me", 0))}.+(("me", me))
    val arr = meMap.values.toArray
    val maxOp = MaxOp()
    permutations(0, arr, maxOp)
    printf("best: %d \n", maxOp.get)
  }

  def main(args: Array[String]) {
    part1_1()
    part2_2()
//    best: 618
//    best: 601
  }
}
