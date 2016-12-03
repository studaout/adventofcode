package studaout.y2015

/**
  * Created by timout on 12/25/15.
  */
object Day21 {

  case class Acc(cost:Int, damage:Int, armor:Int ) {
    def +(a: Acc) = Acc(cost+a.cost, damage+a.damage, armor + a.armor)

    override def toString: String = "cost: " + cost + ", damage: " + damage + ", armor: " + armor
  }
  case class Person(points: Int, damage:Int, armor:Int) {
    def +(a:Acc) = Person(points, damage + a.damage, armor + a.armor)
  }

  val boss = Person(104,8,1)
  val me = Person(100, 0, 0)

  val weapons = List(Acc(8,4,0), Acc(10,5,0), Acc(25,6,0), Acc(40,7,0), Acc(74,8,0))
  val armors = List(Acc(13,0,1),Acc(31,0,2),Acc(53,0,3),Acc(75,0,4),Acc(102,0,5))
  val rings = List(Acc(25,1,0),Acc(50,2,0),Acc(100,3,0),Acc(20,0,1),Acc(40,0,2),Acc(80,0,3))

  val permutRings: List[Acc] = rel(Acc(0,0,0) :: rings)

  def part1(): Unit = {
    val min = weapons.flatMap { w =>
      (Acc(0,0,0) :: armors).map { a => w + a }
    }.flatMap { wa =>
      wa + Acc(0,0,0) :: permutRings.map( wa + _)
    }.foldLeft(Acc(Int.MaxValue,0,0)){ (res, acc) =>
      val curMe = me + acc
      if ( willWin(curMe, boss) ) {
        if ( acc.cost < res.cost) acc else res
      } else res
    }
    println(min)
  }

  def part2(): Unit = {
    val max = weapons.flatMap { w =>
      (Acc(0,0,0) :: armors).map { a => w + a }
    }.flatMap { wa =>
      wa + Acc(0,0,0) :: permutRings.map( wa + _)
    }.foldLeft(Acc(0,0,0)){ (res, acc) =>
      val curMe = me + acc
      //println(acc)
      if ( willLose(curMe, boss) ){
        if ( acc.cost > res.cost) acc else res
      } else res
    }
    println(max)
  }

  def rel( l:List[Acc]): List[Acc] = l match {
    case head :: Nil => Nil
    case head :: rest => rest.map(head + _) ::: rel(rest)
    case _ => Nil
  }

  def willWin(me:Person, boss: Person): Boolean = {
    me.armor+me.damage >= boss.armor+boss.damage &&
    boss.points/(me.damage +boss.armor) <= me.points/(boss.damage+me.armor)
  }

  def willLose(me:Person, boss: Person): Boolean = {
    val min = minMoves(me, boss)
    boss.points - (min*damage(me,boss)) > damage(me,boss)
//    if ( boss.points > me.points ) {
//      boss.points - me.points > damage(me, boss)
//      boss.damage+boss.armor > me.damage + me.armor
//    } else if( boss.points == me.points ) {
//      boss.damage+boss.armor > me.damage + me.armor
//    } else {
//      //
//    }
//    //me.armor+me.damage < boss.armor+boss.damage // ||
//    val bd = if ( me.damage - boss.armor <= 0) 1 else me.damage - boss.armor
//    val md = if ( boss.damage-me.armor <= 0) 1 else boss.damage-me.armor
//    //me.armor+me.damage < boss.armor+boss.damage &&
//      boss.points/bd > me.points/md
  }

  def damage(p1:Person, p2: Person): Int = if ( p1.damage - p2.armor > 0 ) p1.damage - p2.armor else 1
  def minMoves(p1:Person, p2:Person): Int = {
    val d1 = p1.points/damage(p2,p1)
    val d2 = p2.points/damage(p1,p2)
    if ( d1 < d2 ) d1 else d2
  }

  def main(args: Array[String]) {
    part1()
    part2()
  }

}
