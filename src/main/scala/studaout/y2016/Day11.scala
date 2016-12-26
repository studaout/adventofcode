package studaout.y2016

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

import studaout._

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object Day11 {

  implicit val ec = ExecutionContext.global

  val units = Map("first"->0, "second"->1, "third"->2, "fourth"->3)

  trait Device { //11
    val id: Int
    val tp: Int
  }
  case class Generator(id: Int) extends Device { val tp = 1;}
  case class Chip(id: Int) extends Device { val tp = 2; }

  val inputFile = "/2016/day11-input.txt"

  def parse(): (Array[List[Device]], mutable.HashMap[String, Int]) = {
    val floors  =  new Array[List[Device]](4)
    var substance = new mutable.HashMap[String, Int]()
    lines(inputFile).foreach {
      case r"The ([a-z]+)${floor} floor contains nothing relevant\." =>
        println(floor)
        floors(units(floor)) = List()
      case r"The ([a-z]+)${floor} floor contains (.*)${dlist}\." =>
        val device = ",?\\s?(?:and)?\\s?a ([a-z\\-]+) ([a-z]+)".r
        val di = (for (d <- device.findAllMatchIn(dlist)) yield (d.group(1), d.group(2))).toList
        println(di)
        val list = di.map(descr => {
          descr._2 match {
            case "generator" =>
              val devId = substance.getOrElse(descr._1, substance.size)
              substance.put(descr._1, devId)
              Generator(devId)
            case "microchip" =>
              val devId = substance.getOrElse(descr._1.split("-")(0), substance.size)
              substance.put(descr._1.split("-")(0), devId)
              Chip(devId)
          }
        })
        println(list)
        floors(units(floor)) = list
    }
    (floors, substance)
  }

  val (part1Floors, substance) = parse()
  val deviceSize: Int = substance.size * 2
  val addendum = List(Chip(substance.size), Chip(substance.size+1), Generator(substance.size), Generator(substance.size+1))
  val part2Floors: Array[List[Device]] = part1Floors.zipWithIndex.map( l => if ( l._2 == 0 ) l._1::: addendum else l._1)
  val part2DeviceSize: Int = deviceSize + 4

  def mkPairs(lst: List[Device]) : List[List[Device]] = {
    val r = for ( i <- lst; j <- lst) yield (i,j)
    r.filterNot(p => p._1.id >= p._2.id).map( p => List(p._1, p._2) )
  }

  def isValid(floors: Array[List[Device]]) : Boolean = {
    ! floors.exists { floor => isInvalidFloor(floor) }
  }

  def isInvalidFloor(floor: List[Device]) : Boolean = {
    val g = floor.filter(_.tp == 1)
    val c = floor.filter(_.tp == 2)
    if (g.isEmpty || c.isEmpty) false else if (c.size > g.size) true else c.exists(ch => !g.exists(_.id == ch.id))
  }

  def isEnd(floors: Array[List[Device]], size: Int) : Boolean = {
    floors(3).size == size
  }
  def getAllPossibleMoves(floor: Int, floors: Array[List[Device]]): List[List[Device]] = {
    val lst = floors(floor)
    val byGenChip = lst.groupBy(_.id).values.filter(_.size == 2).toList.take(1)
    val by2Gen = mkPairs(lst.filter( _.tp == 1).sortBy(_.id))
    val by2Chip = mkPairs(lst.filter( _.tp == 2).sortBy(_.id))
    val singles = lst.map( d => List(d))
    (singles:::by2Gen:::byGenChip:::by2Chip).filter(m => ! isInvalidFloor(lst.filter( d => ! m.contains(d) )))
  }

  def setBit(pos: Int, state: Long) : Long = state | (1 << pos)
  def unsetBit(pos: Int, state: Long) : Long = state & ~ (1 << pos)

  case class State3(curFloor: Int, floors: Array[List[Device]], num: Int)

  case class CacheState3(curFloor: Int, floors: Long)

  def notInCache3(pos:Int, floors: Array[List[Device]], step: Int, cache: ConcurrentHashMap[CacheState3, Int]): Boolean = {
    val ff = floors.map { f =>
      if ( f.isEmpty ) 0L else {
        f.foldLeft(0L)((s, d) => {
          d match {
            case Chip(id) =>
              setBit(id*2, s)
            case Generator(id) =>
              setBit(id*2+1, s)
          }
        })
      }
    }.foldLeft(0L) ( (s, fp) => {
      (s << 8) + fp
    })
    val st = CacheState3(pos, ff)
    val ps = cache.getOrDefault(st,-1)
    ps == -1 || ps > step
  }

  def putToCache3(pos:Int, floors: Array[List[Device]], step: Int, cache: ConcurrentHashMap[CacheState3, Int]): Unit = {
    val ff = floors.map { f =>
      if ( f.isEmpty ) 0L else {
        f.foldLeft(0L)((s, d) => {
          d match {
            case Chip(id) =>
              setBit(id*2, s)
            case Generator(id) =>
              setBit(id*2+1, s)
          }
        })
      }
    }.foldLeft(0L) ( (s, fp) => {
      (s << 8) + fp
    })
    val st = CacheState3(pos, ff)
    cache.put(st,step)
  }


  def step_3_c(init: Int, initNum: Int, initFloors: Array[List[Device]], finalSize: Int): Unit = {
    var movesList : List[State3] = List(State3(init, initFloors, initNum))
    val cache = new ConcurrentHashMap[CacheState3, Int]()
    putToCache3(0, initFloors, initNum, cache)
    var isDone = new AtomicBoolean(false)
    while( (! isDone.get()) && movesList.nonEmpty ) {
      val groups = movesList.grouped(1000).toList
      movesList = groups.map { g =>
        Future {
          g.flatMap{ move =>
            println(s"state=${move.num}")
            if ( isEnd(move.floors, finalSize) ) {
              println(s" result = ${move.num}")
              isDone.set(true)
              List()
            } else {
              val possibleMoves = getAllPossibleMoves(move.curFloor, move.floors)
              val upList = if (move.curFloor < 3) {
                val nextUpFloor = move.curFloor + 1
                val upDoubleStates = possibleMoves.filter(_.size == 2).map { pm =>
                  val copyFloors = copy(move.floors)
                  copyFloors(nextUpFloor) = pm ::: copyFloors(nextUpFloor)
                  copyFloors(move.curFloor) = copyFloors(move.curFloor).filterNot(d => pm.contains(d))
                  if (isValid(copyFloors) && notInCache3(nextUpFloor, copyFloors, move.num+1, cache)) {
                    val s = State3(nextUpFloor, copyFloors, move.num + 1)
                    putToCache3(nextUpFloor, copyFloors, move.num+1, cache)
                    s
                  } else null
                }.filter(_ != null)
                if (upDoubleStates.isEmpty) {
                  possibleMoves.filter(_.size == 1).map { pm =>
                    val copyFloors = copy(move.floors)
                    copyFloors(nextUpFloor) = pm ::: copyFloors(nextUpFloor)
                    copyFloors(move.curFloor) = copyFloors(move.curFloor).filterNot(d => pm.contains(d))
                    if (isValid(copyFloors) && notInCache3(nextUpFloor, copyFloors, move.num+1, cache)) {
                      val s = State3(nextUpFloor, copyFloors, move.num + 1)
                      putToCache3(nextUpFloor, copyFloors, move.num+1, cache)
                      s
                    } else null
                  }.filter( _ != null)
                } else upDoubleStates
              } else List()

              val downList = if (move.curFloor > 1  || ( move.curFloor == 1 && move.floors(0).nonEmpty) ) {
                val nextDownFloor = move.curFloor - 1
                val downSingleStates = possibleMoves.filter(_.size == 1).map { pm =>
                  val copyFloors = copy(move.floors)
                  copyFloors(nextDownFloor) = pm ::: copyFloors(nextDownFloor)
                  copyFloors(move.curFloor) = copyFloors(move.curFloor).filterNot(d => pm.contains(d))
                  if (isValid(copyFloors) && notInCache3(nextDownFloor, copyFloors, move.num+1, cache) ) {
                    val s = State3(nextDownFloor, copyFloors, move.num + 1)
                    putToCache3(nextDownFloor, copyFloors, move.num+1, cache)
                    movesList = movesList ::: List(s)
                    s
                  } else null
                }.filter(_ != null)
                if (downSingleStates.isEmpty) {
                  possibleMoves.filter(_.size == 2).map { pm =>
                    val copyFloors = copy(move.floors)
                    copyFloors(nextDownFloor) = pm ::: copyFloors(nextDownFloor)
                    copyFloors(move.curFloor) = copyFloors(move.curFloor).filterNot(d => pm.contains(d))
                    if (isValid(copyFloors) && notInCache3(nextDownFloor, copyFloors, move.num+1, cache)) {
                      val s = State3(nextDownFloor, copyFloors, move.num + 1)
                      putToCache3(nextDownFloor, copyFloors, move.num+1, cache)
                      s
                    } else null
                  }.filter(_ != null)
                } else downSingleStates
              } else List()
              upList ::: downList
            }
          }
        }
      }.flatMap { f =>
        Await.ready(f, Duration.Inf)
        f.value.get.get
      }

    }
    if ( ! isDone.get() ) {
      println("Was not able to rich destination:(")
    } else {
      //println(s"final  = ${movesList.head.num}")
    }
  }

  /**
    * You come upon a column of four floors that have been entirely sealed off from the rest of the building except for a small dedicated lobby.
    * There are some radiation warnings and a big sign which reads "Radioisotope Testing Facility".
    * According to the project status board, this facility is currently being used to experiment with Radioisotope Thermoelectric Generators
    * (RTGs, or simply "generators") that are designed to be paired with specially-constructed microchips.
    * Basically, an RTG is a highly radioactive rock that generates electricity through heat.
    * The experimental RTGs have poor radiation containment, so they're dangerously radioactive. The chips are prototypes and don't have normal
    * radiation shielding, but they do have the ability to generate an electromagnetic radiation shield when powered. Unfortunately, they can only
    * be powered by their corresponding RTG. An RTG powering a microchip is still dangerous to other microchips.
    * In other words, if a chip is ever left in the same area as another RTG, and it's not connected to its own RTG, the chip will be fried. Therefore,
    * it is assumed that you will follow procedure and keep chips connected to their corresponding RTG when they're in the same room, and away from
    * other RTGs otherwise.
    * These microchips sound very interesting and useful to your current activities, and you'd like to try to retrieve them. The fourth floor of the
    * facility has an assembling machine which can make a self-contained, shielded computer for you to take with you - that is, if you can bring it
    * all of the RTGs and microchips.
    * Within the radiation-shielded part of the facility (in which it's safe to have these pre-assembly RTGs), there is an elevator that can move
    * between the four floors. Its capacity rating means it can carry at most yourself and two RTGs or microchips in any combination.
    * (They're rigged to some heavy diagnostic equipment - the assembling machine will detach it for you.) As a security measure, the elevator
    * will only function if it contains at least one RTG or microchip. The elevator always stops on each floor to recharge, and this takes long
    * enough that the items within it and the items on that floor can irradiate each other. (You can prevent this if a Microchip and its Generator
    * end up on the same floor in this way, as they can be connected while the elevator is recharging.)
    * You make some notes of the locations of each component of interest (your puzzle input). Before you don a hazmat suit and start moving things
    * around, you'd like to have an idea of what you need to do.
    * When you enter the containment area, you and the elevator will start on the first floor.
    * For example, suppose the isolated area has the following arrangement:
    * The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
    * The second floor contains a hydrogen generator.
    * The third floor contains a lithium generator.
    * The fourth floor contains nothing relevant.
    * As a diagram (F# for a Floor number, E for Elevator, H for Hydrogen, L for Lithium, M for Microchip, and G for Generator), the initial state looks like this:
    * F4 .  .  .  .  .
    * F3 .  .  .  LG .
    * F2 .  HG .  .  .
    * F1 E  .  HM .  LM
    * Then, to get everything up to the assembling machine on the fourth floor, the following steps could be taken:
    * Bring the Hydrogen-compatible Microchip to the second floor, which is safe because it can get power from the Hydrogen Generator:
    * F4 .  .  .  .  .
    * F3 .  .  .  LG .
    * F2 E  HG HM .  .
    * F1 .  .  .  .  LM
    * Bring both Hydrogen-related items to the third floor, which is safe because the Hydrogen-compatible microchip is getting power from its generator:
    * F4 .  .  .  .  .
    * F3 E  HG HM LG .
    * F2 .  .  .  .  .
    * F1 .  .  .  .  LM
    * Leave the Hydrogen Generator on floor three, but bring the Hydrogen-compatible Microchip back down with you so you can still use the elevator:
    * F4 .  .  .  .  .
    * F3 .  HG .  LG .
    * F2 E  .  HM .  .
    * F1 .  .  .  .  LM
    * At the first floor, grab the Lithium-compatible Microchip, which is safe because Microchips don't affect each other:
    * F4 .  .  .  .  .
    * F3 .  HG .  LG .
    * F2 .  .  .  .  .
    * F1 E  .  HM .  LM
    * Bring both Microchips up one floor, where there is nothing to fry them:
    * F4 .  .  .  .  .
    * F3 .  HG .  LG .
    * F2 E  .  HM .  LM
    * F1 .  .  .  .  .
    * Bring both Microchips up again to floor three, where they can be temporarily connected to their corresponding generators while the elevator recharges, preventing either of them from being fried:
    * F4 .  .  .  .  .
    * F3 E  HG HM LG LM
    * F2 .  .  .  .  .
    * F1 .  .  .  .  .
    * Bring both Microchips to the fourth floor:
    * F4 E  .  HM .  LM
    * F3 .  HG .  LG .
    * F2 .  .  .  .  .
    * F1 .  .  .  .  .
    * Leave the Lithium-compatible microchip on the fourth floor, but bring the Hydrogen-compatible one so you can still use the elevator; this is safe because although the Lithium Generator is on the destination floor, you can connect Hydrogen-compatible microchip to the Hydrogen Generator there:
    * F4 .  .  .  .  LM
    * F3 E  HG HM LG .
    * F2 .  .  .  .  .
    * F1 .  .  .  .  .
    * Bring both Generators up to the fourth floor, which is safe because you can connect the Lithium-compatible Microchip to the Lithium Generator upon arrival:
    * F4 E  HG .  LG LM
    * F3 .  .  HM .  .
    * F2 .  .  .  .  .
    * F1 .  .  .  .  .
    * Bring the Lithium Microchip with you to the third floor so you can use the elevator:
    * F4 .  HG .  LG .
    * F3 E  .  HM .  LM
    * F2 .  .  .  .  .
    * F1 .  .  .  .  .
    * Bring both Microchips to the fourth floor:
    * F4 E  HG HM LG LM
    * F3 .  .  .  .  .
    * F2 .  .  .  .  .
    * F1 .  .  .  .  .
    * In this arrangement, it takes 11 steps to collect all of the objects at the fourth floor for assembly.
    * (Each elevator stop counts as one step, even if nothing is added to or removed from it.)
    * In your situation, what is the minimum number of steps required to bring all of the objects to the fourth floor?
    */
  def part1(): Unit = {
    step_3_c(0, 0, part1Floors, deviceSize)
  }

  /**
    * You step into the cleanroom separating the lobby from the isolated area and put on the hazmat suit.
    * Upon entering the isolated containment area, however, you notice some extra parts on the first floor that weren't listed on the record outside:
    * An elerium generator.
    * An elerium-compatible microchip.
    * A dilithium generator.
    * A dilithium-compatible microchip.
    * These work just like the other generators and microchips. You'll have to get them up to assembly as well.
    * What is the minimum number of steps required to bring all of the objects, including these four new ones, to the fourth floor?
    */
  def part2(): Unit = {
    step_3_c(0,0,part2Floors, part2DeviceSize)
  }

  def copy(floors: Array[List[Device]]): Array[List[Device]] = {
    floors.toList.toArray
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

}
