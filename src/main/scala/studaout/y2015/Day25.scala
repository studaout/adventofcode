package studaout.y2015

/**
  * Created by timout on 12/30/15.
  */
object Day25 {

  def index(row: Int, col: Int): Int = {
    val n = row + col
    val lastInRow = n * (n-1) / 2 //last element in the triangle row (sum all nums till n)
    lastInRow - row  // last element - row +1
  }

  def part1_1(): Unit = { //using Modular exponentiation
    val n = index(3010, 3019)
    val res = 20151125L * BigInt(252533).modPow(n, 33554393) % 33554393
    println(n + ": " + res)
  }

  def part1(): Unit ={
    val n = index(3010, 3019)
    val res = (1 to n ).foldLeft(20151125L) { (res, v)=> (res * 252533) % 33554393}
    println(n + ": " + res)
  }


  def main(args: Array[String]) {
    part1() //18168396: 8997277
    part1_1()
    // 7 -> 5 mod 3; ind 4
  }

}
