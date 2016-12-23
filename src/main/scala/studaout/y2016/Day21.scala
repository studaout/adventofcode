package studaout.y2016

import studaout._

object Day21 {

  val inputFile = "/2016/day21-input.txt"

  val passwd = "abcdefgh"
  val size = passwd.size

  val scrambled = "fbgdceah"

//  val instructions = lines(inputFile).map{ line =>
//    //
//  }


  def part1(): Unit = {
    var str : StringBuilder = new StringBuilder(passwd)
    lines(inputFile).foreach {
      case r"rotate right (\d)${steps} step.?" =>
        for ( i <- 1 to steps.toInt) {
          val ch = str.charAt(size-1)
          str.deleteCharAt(size-1)
          str.insert(0, ch)
        }
      case r"rotate left (\d)${steps} step.?" =>
        for ( i <- 1 to steps.toInt) {
          val ch = str.charAt(0)
          str.deleteCharAt(0)
          str.append(ch)
        }
      case r"rotate based on position of letter (\w)${ch}" =>
        val s = str.indexOf(ch.charAt(0)) + 1
        val steps = if ( s > 4) s + 1 else s
        for ( i <- 1 to steps) {
          val ch = str.charAt(size-1)
          str.deleteCharAt(size-1)
          str.insert(0, ch)
        }
      case r"swap position (\d)${p1} with position (\d)${p2}" =>
        val c1 = str.charAt(p1.toInt)
        str.setCharAt(p1.toInt, str.charAt(p2.toInt))
        str.setCharAt(p2.toInt, c1)
      case r"swap letter (\w)${ch1} with letter (\w)${ch2}" =>
        val ind1 = str.indexOf(ch1.charAt(0))
        val ind2 = str.indexOf(ch2.charAt(0))
        str.update(ind1, ch2.charAt(0))
        str.update(ind2, ch1.charAt(0))
      case r"reverse positions (\d)${p1} through (\d)${p2}" =>
        val s = str.substring(p1.toInt,p2.toInt+1).reverse
        str.replace(p1.toInt,p2.toInt+1, s)
      case r"move position (\d)${p1} to position (\d)${p2}" =>
        val c1 = str.charAt(p1.toInt)
        str.deleteCharAt(p1.toInt)
        str.insert(p2.toInt, c1)
    }
    println(str.toString())
  }

  def part2(): Unit = {
    var str : StringBuilder = new StringBuilder(scrambled)
    lines(inputFile).toList.reverse.foreach {
      case r"rotate right (\d)${steps} step.?" =>
        for ( i <- 1 to steps.toInt) {
          val ch = str.charAt(0)
          str.deleteCharAt(0)
          str.append(ch)
        }
      case r"rotate left (\d)${steps} step.?" =>
        for ( i <- 1 to steps.toInt) {
          val ch = str.charAt(size-1)
          str.deleteCharAt(size-1)
          str.insert(0, ch)
        }
      case r"rotate based on position of letter (\w)${ch}" => //
        val s = str.indexOf(ch.charAt(0))
        val steps = s match { //0-1-1, 1-2-3, 2-3-5-,
          case 1 => 1
          case 3 => 2
          case 5 => 3
          case 7 => 4
          case 2 => 6
          case 4 => 7
          case 6 => 0
          case 0 => 1
        }
        for ( i <- 1 to steps.toInt) {
          val ch = str.charAt(0)
          str.deleteCharAt(0)
          str.append(ch)
        }
      case r"swap position (\d)${p1} with position (\d)${p2}" =>
        val c1 = str.charAt(p1.toInt)
        str.setCharAt(p1.toInt, str.charAt(p2.toInt))
        str.setCharAt(p2.toInt, c1)
      case r"swap letter (\w)${ch1} with letter (\w)${ch2}" =>
        val ind1 = str.indexOf(ch1.charAt(0))
        val ind2 = str.indexOf(ch2.charAt(0))
        str.update(ind1, ch2.charAt(0))
        str.update(ind2, ch1.charAt(0))
      case r"reverse positions (\d)${p1} through (\d)${p2}" => //
        val s = str.substring(p1.toInt,p2.toInt+1).reverse
        str.replace(p1.toInt,p2.toInt+1, s)
      case r"move position (\d)${p1} to position (\d)${p2}" =>
        val c2 = str.charAt(p2.toInt)
        str.deleteCharAt(p2.toInt)
        str.insert(p1.toInt, c2)
    }
    println(str.toString())
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

}
