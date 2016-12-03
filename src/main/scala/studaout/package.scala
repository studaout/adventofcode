import scala.io.Source

/**
  * Created by timout on 12/1/16.
  */
package object studaout {

  def lines(path: String): Iterator[String] = Source.fromInputStream(getClass.getResourceAsStream(path)).getLines()
  def firstLine(path: String) : String = lines(path).next()


  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }


}
