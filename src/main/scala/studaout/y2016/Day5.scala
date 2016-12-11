package studaout.y2016

import java.security.MessageDigest
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue, Semaphore}

import scala.collection._
import scala.collection.JavaConverters._

import scala.compat.Platform
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Day5 {

  val doorID = "cxdnnyjw"

  def part1(): Unit = {
    var i = 0
    var j = 0
    var pwd = ""
    while ( i < 8 && j < 100000) {
      val str = doorID + j.toString
      val hash = MessageDigest.getInstance("MD5").digest(str.getBytes).map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}
      if ( hash.startsWith("00000")) {
        pwd = pwd + hash.charAt(5)
        i = i + 1
        println(s"i=$i pwd=$pwd")
      }
      j = j + 1
    }
    println(pwd)
  }

  def part1p(): Unit = {
    println("---------------------")
    val s = Platform.currentTime
    val resList = new ConcurrentLinkedQueue[Int]()
    val MAX_AVAILABLE = 8
    val step = 1000
    val available = new Semaphore(MAX_AVAILABLE, true)

    val counter = new AtomicInteger(0)

    Stream.from(1,step).find(i => {
      available.acquire()
      if ( resList.isEmpty ) {
        val f: Future[Int] = Future {
          val digest = MessageDigest.getInstance("MD5")
          val res = Stream.range(i, i + step).find(cur => {
            val str = doorID + cur.toString
            val hash = MessageDigest.getInstance("MD5").digest(str.getBytes).map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}
            //            println(str + " : " + md5)
            if (hash.startsWith("00000")) {
              //println("thread final " + str + " : " + hash)
              println(s"$hash  - $cur")
              val curcounter = counter.incrementAndGet()
              if ( curcounter > 7 ) true else false
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
    println("execution time :" + f) //f77a0e6e
  }

  def part2p(): Unit = {
    println("---------------------")
    val s = Platform.currentTime
    val resList = new ConcurrentLinkedQueue[Int]()
    val MAX_AVAILABLE = 8
    val step = 1000
    val available = new Semaphore(MAX_AVAILABLE, true)

    val counter = new AtomicInteger(0)
    val map: concurrent.Map[Char, Char] = new ConcurrentHashMap[Char, Char]().asScala

    Stream.from(2503850,step).find(i => {
      available.acquire()
      if ( resList.isEmpty ) {
        val f: Future[Int] = Future {
          val digest = MessageDigest.getInstance("MD5")
          val res = Stream.range(i, i + step).find(cur => {
            val str = doorID + cur.toString
            val hash = MessageDigest.getInstance("MD5").digest(str.getBytes).map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}
            //            println(str + " : " + md5)
            if (hash.startsWith("00000")) {
              val pos = hash.charAt(5)
              if ( pos >= '0' && pos <'8' ) {
                val ch = hash.charAt(6)
                if ( ! map.contains(pos) ) {
                  map.put(pos,ch)
                  println(s"$hash  - $cur")
                  val curcounter = counter.incrementAndGet()
                  if ( curcounter > 7 ) true else false
                } else {
                  false
                }
              } else {
                false
              }
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
    println("execution time :" + f) //f77a0e6e  999828ec
  }

  def main(args: Array[String]): Unit = {
    part2p()
  }

}
