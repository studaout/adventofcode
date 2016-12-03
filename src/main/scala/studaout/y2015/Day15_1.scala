package studaout.y2015

import scala.collection.mutable
import studaout._

class Day15_1 {
  val pattern = "(?<i>[A-Za-z]+): capacity (?<cap>[-0-9]+), durability (?<dur>[-0-9]+), flavor (?<fla>[-0-9]+), texture (?<tex>[-0-9]+), calories (?<cal>[-0-9]+)".r


  def getResource(file: String): List[String] = {
    lines(file).toList
  }

  def printAnswer(day: Int, section: String, result: Any): Unit = {
    printf("Day %2s / %s: %10s\n", day, section, result)
  }

  def run(): Unit = {

    val ingredients = getResource(Day15_1.inputFile).map(s =>
      s match {
        case pattern(n, cap, dur, fla, tex, cal) =>
          Ingredient(n, cap.toInt, dur.toInt, fla.toInt, tex.toInt, cal.toInt)})

    val combinations = mutable.MutableList.empty[List[Int]]

    makeCombinations(100, 0, combinations, ingredients.map(i => 0))

    printAnswer(15, "One", combinations.map(l => sumAll(ingredients, l)).max)
    printAnswer(15, "Two", combinations.filter(l => sum(ingredients, l, i => i.calories) == 500).map(l => sumAll(ingredients, l)).max)
  }

  def makeCombinations(target: Int, index: Int, results: mutable.MutableList[List[Int]], list: List[Int]) : Unit = {
    for(i <- 0 until target) {
      val newList = list.patch(index, Seq(i), 1)

      if(index < list.size - 1 && newList.sum <= target) {
        makeCombinations(target, index + 1, results, newList)
      }
      if(index == list.size - 1 && newList.sum == target) {
        results += newList
      }
    }
  }

  def sum(ingredients: List[Ingredient], amounts: List[Int], property: Ingredient => Int) = {
    Math.max(ingredients.zip(amounts).map(t => property(t._1) * t._2).sum, 0)
  }

  def sumAll(ingredients: List[Ingredient], amounts: List[Int]) = {
    sum(ingredients, amounts, i => i.capacity) * sum(ingredients, amounts, i => i.durability) * sum(ingredients, amounts, i => i.flavor) * sum(ingredients, amounts, i => i.texture)
  }

  case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)

}

object Day15_1 {

  val inputFile = "/2015/day15-input.txt"

  def main(args: Array[String]) {
    val d = new Day15_1()
    d.run()
  }
}
