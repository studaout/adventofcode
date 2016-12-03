package studaout.y2015

import java.util.concurrent.Semaphore
import java.util.concurrent.atomic.AtomicLong

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import studaout._

object Day15 {

  val inputFile = "/2015/day15-input.txt"

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  case class Ing(name:String, cap:Int, dur:Int, fla:Int, tex:Int, cal:Int) {
    def res : Long = Array(cap,dur,fla,tex).foldLeft(1){ (a,b) => if ( b <=0 ) 0 else a*b }
    def +(i:Ing) = Ing(name, cap + i.cap, dur + i.dur, fla + i.fla, tex+i.tex, cal + i.cal)
    def *(size:Int) = Ing(name, cap*size, dur*size, fla*size, tex*size, cal*size)
  }

  def part1(): Unit = {
    //Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5
    val arr = lines(inputFile).map {
      case r"([a-zA-Z]+)${name}: capacity (-?\d+)${cap}, durability (-?\d+)${dur}, flavor (-?\d+)${fla}, texture (-?\d+)${tex}, calories (-?\d+)${cal}" =>
        Ing(name, cap.toInt, dur.toInt, fla.toInt, tex.toInt, cal.toInt)
      case default => throw new Exception("unknown: " + default )
    }.toArray

    val size = 100 - arr.length
    val len = arr.length
    val mem = new Array[Ing](size+1)
    mem(0) = arr.foldLeft(Ing("res", 0, 0, 0, 0, 0)){ (a,b) => a + b}
    for (i <- 1 to size ) {

      val cur = arr.flatMap{ ing =>
        val s = if ( i < len ) i else len
        for ( j <- 1 to s ) yield mem(i-j) + (ing*j)
      }.maxBy( r => r.res )

//      val cur = arr.maxBy{ ing =>
//        val s = if ( i < len ) i else len
//        val cl = for ( j <- 1 to s ) yield mem(i-j) + (ing*j)
//        val locMax = cl.maxBy( r => r.res )
//        locMax.res
//      }
      mem(i) = cur
    }
    printf("total: %d\n", mem(size).res)
  }

  case class Ing2(name:String, cap:Int, dur:Int, fla:Int, tex:Int, cal:Int, s:Int) {
    def res : Long = Array(cap,dur,fla,tex).foldLeft(1){ (a,b) => if ( b <=0 ) 0 else a*b }
    def +(i:Ing2) = Ing2(name, cap + i.cap, dur + i.dur, fla + i.fla, tex+i.tex, cal + i.cal, s+1)
    def -(i:Ing2) = Ing2(name, cap - i.cap, dur - i.dur, fla - i.fla, tex - i.tex, cal - i.cal, s-1)
    def size = s
    def copy() = Ing2(name, cap, dur, fla, tex, cal, s)
    //def *(size:Int) = Ing2(name, cap*size, dur*size, fla*size, tex*size, cal*size)
  }

  class MaxCal extends Function1[Ing2, Ing2] {
    private var m = Ing2("", 0,0,0,0,0,0)
    var count = new AtomicLong(0)
    override def apply(i:Ing2) : Ing2 = {
      val cc = count.incrementAndGet()
      if ( cc % 1000000000 == 0 ) {
        printf("count: %d \n", cc)
        val r = m.res
        if ( r > 0 ) printf("cur max: %d \n", r)
      }
      if ( i.size == 100 && i.cal == 500 && i.res > m.res) {
        synchronized {
          printf("res: %d, cal: %d \n", i.res, i.cal)
          m = i.copy()
        }
      }
      m
    }
    def max = m
  }

  def part2(): Unit = {
    val arr = lines(inputFile).map {
      case r"([a-zA-Z]+)${name}: capacity (-?\d+)${cap}, durability (-?\d+)${dur}, flavor (-?\d+)${fla}, texture (-?\d+)${tex}, calories (-?\d+)${cal}" =>
        Ing2(name, cap.toInt, dur.toInt, fla.toInt, tex.toInt, cal.toInt, 1)
      case default => throw new Exception("unknown: " + default )
    }.toArray
    val s = arr.foldLeft(Ing2("res", 0, 0, 0, 0, 0, 0)){ (a,b) => a + b}
    val mf = new MaxCal()
    val sem = new Semaphore(arr.length)
    sem.acquire(arr.length)
    for ( i <- arr ) {
      Future {
        part2_rec(s+i, arr, mf)
      }.onComplete {
        r =>
          println("completed: " + r.isSuccess)
          sem.release()
      }
    }
    sem.acquire(arr.length)
    //part2_rec(i, arr, mf)
    printf("max with cal: %d \n", mf.max.res)
  }

  def part2_rec(cur:Ing2, arr:Array[Ing2], f:MaxCal): Unit = {
    if ( cur.size >= 100 ) {
      //printf("res: %d, cal: %d \n", cur.res, cur.cal)
      f(cur)
    } else {
      for ( i <- arr ) {
        part2_rec(cur+i, arr, f)
      }
    }
  }


  def main(args: Array[String]) {
    part1()
    part2()
  }

}
