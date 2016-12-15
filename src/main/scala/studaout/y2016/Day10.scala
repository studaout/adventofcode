package studaout.y2016

import studaout._

import scala.collection.mutable

object Day10 {

  /**
    * You come upon a factory in which many robots are zooming around handing small microchips to each other.
    * Upon closer examination, you notice that each bot only proceeds when it has two microchips, and once it does,
    * it gives each one to a different bot or puts it in a marked "output" bin. Sometimes, bots take microchips from "input" bins, too.
    *
    * Inspecting one of the microchips, it seems like they each contain a single number; the bots must use some logic to decide
    * what to do with each chip. You access the local control computer and download the bots' instructions (your puzzle input).
    *
    * Some of the instructions specify that a specific-valued microchip should be given to a specific bot; the rest of the instructions
    * indicate what a given bot should do with its lower-value or higher-value chip.
    *
    * For example, consider the following instructions:
    * value 5 goes to bot 2
    * bot 2 gives low to bot 1 and high to bot 0
    * value 3 goes to bot 1
    * bot 1 gives low to output 1 and high to bot 0
    * bot 0 gives low to output 2 and high to output 0
    * value 2 goes to bot 2
    *
    * Initially, bot 1 starts with a value-3 chip, and bot 2 starts with a value-2 chip and a value-5 chip.
    * Because bot 2 has two microchips, it gives its lower one (2) to bot 1 and its higher one (5) to bot 0.
    * Then, bot 1 has two microchips; it puts the value-2 chip in output 1 and gives the value-3 chip to bot 0.
    * Finally, bot 0 has two microchips; it puts the 3 in output 2 and the 5 in output 0.
    *
    * In the end, output bin 0 contains a value-5 microchip, output bin 1 contains a value-2 microchip, and output bin 2 contains
    * a value-3 microchip. In this configuration, bot number 2 is responsible for comparing value-5 microchips with value-2 microchips.
    */


  trait Action {
    def id : Int
  }
  case class InputAction(id: Int, botNum: Int, value:Int) extends Action
  case class GiveToBotAction(id: Int, lowNum:Int, highNum: Int) extends Action
  case class GiveToOutputBotAction(id: Int, lowNum:Int, highNum: Int) extends Action
  case class GiveToBotOutputAction(id: Int, lowNum:Int, highNum: Int) extends Action
  case class GiveToOutputAction(id: Int, lowNum:Int, highNum: Int) extends Action

  case class Bot(id:Int, action: Action, state: mutable.HashMap[Int, Bot], outState: mutable.HashMap[Int, Int]) {
    private var low: Int = -1
    private var high: Int = -1
    def isEnd : Boolean = low == 17 && high == 61
    def nonEmpty : Boolean = low > -1 && high > -1
    def empty : Boolean = low == -1 || high == -1

    def setValue(value : Int): Unit = {
      var n1 = low
      var n2 = high
      if ( n1 == -1 ) n1 = value else n2 = value
      low = Math.min(n1,n2)
      high = Math.max(n1,n2)
      if ( nonEmpty ) {
        if ( isEnd ) println(id)
      }
    }

    def execute() : Unit = {
      if ( nonEmpty ) {
        if ( isEnd ) println(id)
        act()
      }
    }

    def act() : Unit = {
      action match {
        case GiveToBotAction(actId, lowNum, highNum) =>
          val l = state(lowNum)
          l.setValue(low)
          val h = state(highNum)
          h.setValue(high)
          low = -1
          high = -1
        case GiveToOutputBotAction(actId, lowNum, highNum) =>
          val h = state(highNum)
          h.setValue(high)
          outState(lowNum) = low
          low = -1
          high = -1
        case GiveToBotOutputAction(actId, lowNum, highNum) =>
          val l = state(lowNum)
          l.setValue(low)
          outState(highNum) = high
          low = -1
          high = -1
        case GiveToOutputAction(actId, lowNum, highNum) =>
          outState(lowNum) = low
          outState(highNum) = high
          low = -1
          high = -1
      }
    }
  }

  val inputFile = "/2016/day10-input.txt"

  def parse(outState: mutable.HashMap[Int, Int]) : (List[InputAction], mutable.HashMap[Int, Bot]) = {
    lines(inputFile).zipWithIndex.foldLeft((List[InputAction](), new mutable.HashMap[Int, Bot]()))((s, line) => {
      line._1 match {
        case r"value (\d+)${value} goes to bot (\d+)${botNum}" =>
          (InputAction(line._2, botNum.toInt, value.toInt) :: s._1, s._2)
        case r"bot (\d+)${botNum} gives low to bot (\d+)${lowBotNum} and high to bot (\d+)${highBotNum}" =>
          val a = GiveToBotAction(line._2, lowBotNum.toInt, highBotNum.toInt)
          val b = Bot(botNum.toInt, a, s._2, outState)
          s._2.put(b.id, b)
          s
        case r"bot (\d+)${botNum} gives low to output (\d+)${lowBotNum} and high to bot (\d+)${highBotNum}" =>
          val a = GiveToOutputBotAction(line._2, lowBotNum.toInt, highBotNum.toInt)
          val b = Bot(botNum.toInt, a, s._2, outState)
          s._2.put(b.id, b)
          s
        case r"bot (\d+)${botNum} gives low to bot (\d+)${lowBotNum} and high to output (\d+)${highBotNum}" =>
          val a = GiveToBotOutputAction(line._2, lowBotNum.toInt, highBotNum.toInt)
          val b = Bot(botNum.toInt, a, s._2, outState)
          s._2.put(b.id, b)
          s
        case r"bot (\d+)${botNum} gives low to output (\d+)${lowBotNum} and high to output (\d+)${highBotNum}" =>
          val a = GiveToOutputAction(line._2, lowBotNum.toInt, highBotNum.toInt)
          val b = Bot(botNum.toInt, a, s._2, outState)
          s._2.put(b.id, b)
          s
      }
    })
  }

  // Based on your instructions, what is the number of the bot that is responsible for comparing value-61 microchips with value-17 microchips?
  def part1(): Unit = {
    val outState = new mutable.HashMap[Int, Int]()
    val parseRes = parse(outState)
    val botMap = parseRes._2
    val actions = parseRes._1.sortBy(_.botNum)
    actions.foreach { action =>
      val b = botMap(action.botNum)
      b.setValue(action.value)
    }
    var bots = botMap.filter(_._2.nonEmpty).values.toList.sortBy(_.id) //sort??
    while( bots.nonEmpty) {
      bots.foreach ( _.execute())
      bots = botMap.filter(_._2.nonEmpty).values.toList.sortBy(_.id) //sort??
    }

  }

  //What do you get if you multiply together the values of one chip in each of outputs 0, 1, and 2?
  def part2(): Unit = {
    val outState = new mutable.HashMap[Int, Int]()
    val parseRes = parse(outState)
    val botMap = parseRes._2
    val actions = parseRes._1.sortBy(_.botNum)
    actions.foreach { action =>
      val b = botMap(action.botNum)
      b.setValue(action.value)
    }
    var bots = botMap.filter(_._2.nonEmpty).values.toList.sortBy(_.id) //sort??
    while( bots.nonEmpty) {
      bots.foreach ( _.execute())
      bots = botMap.filter(_._2.nonEmpty).values.toList.sortBy(_.id) //sort??
    }
    val res = outState(0)*outState(1)*outState(2)
    println(res)

  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

}

