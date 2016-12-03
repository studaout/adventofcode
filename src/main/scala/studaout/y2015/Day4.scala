package studaout.y2015

import java.security.MessageDigest
import java.util.concurrent.{ConcurrentLinkedQueue, Semaphore}

import scala.compat.Platform
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
//import scala.collection.JavaConversions._
//import scala.concurrent.ExecutionContext.Implicits.global



object Day4 {


  val prefix = "bgvyzdsv"

  def part1(): Unit = {
    Stream.from(1).find(i => {
      val str = prefix + i.toString
      val digest = MessageDigest.getInstance("MD5")
      val md5 = digest.digest(str.getBytes).map("%02x".format(_)).mkString
      //println(str + " : " + md5 )
      if ( md5.startsWith("00000")) {
        println(str + " : " + md5 )
        true
      } else false
    })
  }

  def part2(): Unit = {
    println("---------------------")
    val s = Platform.currentTime
    Stream.from(1).find(i => {
      val str = prefix + i.toString
      val digest = MessageDigest.getInstance("MD5")
      val md5 = digest.digest(str.getBytes).map("%02x".format(_)).mkString
      //println(str + " : " + md5 )
      if ( md5.startsWith("000000")) {
        println(str + " : " + md5 )
        true
      } else false
    })
    val f = Platform.currentTime - s
    println("execution time :" + f)
  }

  def part2p(): Unit = {
    println("---------------------")
    val s = Platform.currentTime
    val resList = new ConcurrentLinkedQueue[Int]()
    val MAX_AVAILABLE = 8
    val step = 1000
    val available = new Semaphore(MAX_AVAILABLE, true)

    Stream.from(1,step).find(i => {
      available.acquire()
      if ( resList.isEmpty ) {
        val f: Future[Int] = Future {
          val digest = MessageDigest.getInstance("MD5")
          val res = Stream.range(i, i + step).find(cur => {
            val str = prefix + cur.toString
            val md5 = digest.digest(str.getBytes).map("%02x".format(_)).mkString
//            println(str + " : " + md5)
            if (md5.startsWith("000000")) {
              println("thread final " + str + " : " + md5)
              true
            } else false
          })
          res.getOrElse(0)
        }
        f.onComplete(r => {
          r.foreach(v => if (v > 0) resList.add(v))
          available.release()
        })
        false
      } else {
        available.release()
        true
      }
    })
    available.acquire(MAX_AVAILABLE)
    import scala.collection.JavaConversions._
    val res = resList.toList.sorted.get(0)
    val f = Platform.currentTime - s
    println("result: " + res)
    println("execution time :" + f)
  }

  def main(args: Array[String]) {
    //part1()
    //part2() //bgvyzdsv1038736 : 000000b1b64bf5eb55aad89986126953
    part2p()
  }
}
