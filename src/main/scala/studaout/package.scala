import scala.io.Source

/**
  * Created by timout on 12/1/16.
  */
package object studaout {

  def firstLine(path: String) : String = Source.fromInputStream(getClass.getResourceAsStream(path)).getLines().next()

}
