package studaout.y2016

import studaout._

object Day4 {

  def inputFile = "/2016/day4-input.txt"

  /**
    * Finally, you come across an information kiosk with a list of rooms. Of course, the list is encrypted and full of decoy data,
    * but the instructions to decode the list are barely hidden nearby. Better remove the decoy data first.
    * Each room consists of an encrypted name (lowercase letters separated by dashes) followed by a dash, a sector ID, and a checksum in square brackets.
    * A room is real (not a decoy) if the checksum is the five most common letters in the encrypted name,
    * in order, with ties broken by alphabetization. For example:
    * aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters are a (5), b (3), and then a tie between x, y, and z, which are listed alphabetically.
    * a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters are all tied (1 of each), the first five are listed alphabetically.
    * not-a-real-room-404[oarel] is a real room.
    * totally-real-room-200[decoy] is not.
    * Of the real rooms from the list above, the sum of their sector IDs is 1514.
    * What is the sum of the sector IDs of the real rooms?
    */

  def part1(): Unit = {
    val r = lines(inputFile).foldLeft(0)( (sum, line) => {
      val res = line.foldLeft( (Map[Char, Int](), "", "", false)) ( (state, ch) => { //map, id, checksum
        ch match {
          case d if d.isDigit =>
            (state._1, state._2 + ch, state._3, true)
          case '-' => state
          case '[' => state
          case ']' => state
          case _ =>
            if ( state._4 ) (state._1, state._2 , state._3 + ch, true) else {
              val cnt = state._1.getOrElse(ch, 0) + 1
              (state._1 + (ch -> cnt), state._2 , state._3, false)
            }
        }
      })
      val first = res._1.toList.sortWith( (p1,p2) => {
        ( p1._2 > p2._2 ) || ( (p1._2 == p2._2) && (p1._1 < p2._1) )
      }).take(5)
      val id = res._2.toInt
      val add = res._3.foldLeft( ("", id, first.map(_._1))) ( (s, ch) => {
        if ( ! s._3.contains(ch) ) ("", 0, s._3) else {
          val firstCh = s._3.filterNot( fc => fc == ch)
          if ( first.contains( (ch,1)) ) (s._1 + ch, s._2, firstCh) else (s._1, s._2, firstCh)
        }
      })
      if ( ! add._1.isEmpty && add._1 != add._1.sorted) sum else sum + add._2
    })
    println(r)
  }

  /**
    * The room names are encrypted by a state-of-the-art shift cipher, which is nearly unbreakable without the right software.
    * However, the information kiosk designers at Easter Bunny HQ were not expecting to deal with a master cryptographer like yourself.
    * To decrypt a room name, rotate each letter forward through the alphabet a number of times equal to the room's sector ID.
    * A becomes B, B becomes C, Z becomes A, and so on. Dashes become spaces.
    * For example, the real name for qzmt-zixmtkozy-ivhz-343 is very encrypted name.
    * What is the sector ID of the room where North Pole objects are stored?
    */
  def part2(): Unit = {

    val r = lines(inputFile).foreach { line =>
      val res = line.foldLeft((Map[Char, Int](), "", "", false))((state, ch) => {
        //map, id, checksum
        ch match {
          case d if d.isDigit =>
            (state._1, state._2 + ch, state._3, true)
          case '-' => state
          case '[' => state
          case ']' => state
          case _ =>
            if (state._4) (state._1, state._2, state._3 + ch, true) else {
              val cnt = state._1.getOrElse(ch, 0) + 1
              (state._1 + (ch -> cnt), state._2, state._3, false)
            }
        }
      })
      val first = res._1.toList.sortWith((p1, p2) => {
        (p1._2 > p2._2) || ((p1._2 == p2._2) && (p1._1 < p2._1))
      }).take(5)
      val id = res._2.toInt
      val add = res._3.foldLeft(("", id, first.map(_._1)))((s, ch) => {
        if (!s._3.contains(ch)) ("", 0, s._3) else {
          val firstCh = s._3.filterNot(fc => fc == ch)
          if (first.contains((ch, 1))) (s._1 + ch, s._2, firstCh) else (s._1, s._2, firstCh)
        }
      })
      if ((add._1.isEmpty || add._1 == add._1.sorted) && (add._2 > 0)) {
        val rotationNum = id % 26
        val r = line.foldLeft(("", false))((s, ch) => {
          if (s._2) s else {
            ch match {
              case '-' => (s._1 + ' ', false)
              case d if d.isDigit => (s._1, true)
              case _ =>
                val resCh = (1 to rotationNum).foldLeft(ch)((dch, i) => if (dch == 'z') 'a' else (dch + 1).toChar)
                (s._1 + resCh, false)
            }
          }
        })
        if ( r._1 == "northpole object storage " ) {
          println(id)
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

}
