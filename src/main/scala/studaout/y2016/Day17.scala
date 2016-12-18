package studaout.y2016

import java.security.MessageDigest

import studaout._

object Day17 {

  val inputFile = "/2016/day17-input.txt"

  val init: String = firstLine(inputFile)

  def hash(str: String) : String = {
    MessageDigest.getInstance("MD5").digest(str.getBytes).map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}.substring(0,4)
  }

  case class State(x:Int, y: Int, path:String)

  /**
    * The doors in your current room are either open or closed (and locked) based on the hexadecimal MD5 hash of a passcode
    * (your puzzle input) followed by a sequence of uppercase characters representing the path you have taken so far
    * (U for up, D for down, L for left, and R for right).
    * Only the first four characters of the hash are used; they represent, respectively, the doors up, down, left, and
    * right from your current position. Any b, c, d, e, or f means that the corresponding door is open; any other character
    * (any number or a) means that the corresponding door is closed and locked.
    * To access the vault, all you need to do is reach the bottom-right room; reaching this room opens the vault and all doors in the maze.
    *
    * Given your vault's passcode, what is the shortest path (the actual path, not just the length) to reach the vault?
    */
  def part1(): Unit = {
    var isExit = false
    var states: List[State] = List(State(0,0,init))
    while( ! isExit ) {
      val state = states.head
      if ( state.x == 3 && state.y == 3 ) {
        isExit = true
        println(state.path)
      } else {
        val h = hash(state.path)
        val next = h.zipWithIndex.map{ ch =>
          if ( ch._1 == 'b' || ch._1 == 'c' || ch._1 == 'd' || ch._1 == 'e' || ch._1 == 'f' ) {
            ch._2 match {
              case 0 =>
                if ( state.y > 0 ) State(state.x, state.y-1, state.path+'U') else null
              case 1 =>
                if ( state.y < 3 ) State(state.x, state.y+1, state.path+'D') else null
              case 2 =>
                if ( state.x > 0 ) State(state.x-1, state.y, state.path+'L') else null
              case 3 =>
                if ( state.x < 3 ) State(state.x+1, state.y, state.path+'R') else null
            }
          } else {
            null
          }
        }.filter( _ != null ).toList
        states = states.tail ::: next
      }
    }
  }

  /**
    * What is the length of the longest path that reaches the vault?
    */
  def part2(): Unit = {
    var isExit = false
    var states: List[State] = List(State(0,0,init))
    var max = 0
    while( ! isExit ) {
      if ( states.isEmpty ) {
        isExit = true
      } else {
        val state = states.head
        if (state.x == 3 && state.y == 3) {
          max = Math.max(max, state.path.length-init.length)
          println(max)
          states = states.tail
        } else {
          val h = hash(state.path)
          val next = h.zipWithIndex.map { ch =>
            if (ch._1 == 'b' || ch._1 == 'c' || ch._1 == 'd' || ch._1 == 'e' || ch._1 == 'f') {
              ch._2 match {
                case 0 =>
                  if (state.y > 0) State(state.x, state.y - 1, state.path + 'U') else null
                case 1 =>
                  if (state.y < 3) State(state.x, state.y + 1, state.path + 'D') else null
                case 2 =>
                  if (state.x > 0) State(state.x - 1, state.y, state.path + 'L') else null
                case 3 =>
                  if (state.x < 3) State(state.x + 1, state.y, state.path + 'R') else null
              }
            } else {
              null
            }
          }.filter(_ != null).toList
          states = states.tail ::: next
        }
      }
    }
    println(max)
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

}
