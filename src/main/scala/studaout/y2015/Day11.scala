package studaout.y2015


object Day11 {

  def nextPwd(str:String) : String = {
    var i = str.length - 1
    val arr = str.toCharArray
    while ( i >= 0 ) {
      val c = (arr(i) + 1).toChar
      if ( c <= 'z' ) {
        arr(i) = c
        i = 0
      } else {
        arr(i) = 'a'
      }
      i -= 1
    }
    new String(arr)
  }

  def isValid(str: String) : Boolean = {
    val prohibited = Set('i', 'o', 'l')
    val isProhibited = str.exists(c => prohibited(c))
    var repeat = 0
    var seq = 0
    for (i <- 2 until str.length) {
      if ( (str.charAt(i-2)+2).toChar == str.charAt(i) && (str.charAt(i-1)+1).toChar == str.charAt(i) ) seq +=1
    }
    val cache = collection.mutable.HashMap[String, Int]()
    for (i <- 1 until str.length) {
      if ( str.charAt(i-1) ==  str.charAt(i) ) {
        val s = str.substring(i-1, i+1)
        if ( ! cache.contains(s) ) {
          repeat +=1
          cache.put(s,1)
        }
      }
    }
    !isProhibited && seq > 0 && repeat > 1
  }

  def pwdStream(init:String) : Stream[String] = {
    def l(str:String) :Stream[String] = {
      val p = nextPwd(str)
      p #:: l(p)
    }
    l(init)
  }

  def part1(): Unit = {
    val pwd = "cqjxjnds"
    val n = pwdStream(pwd).find(p => isValid(p)).getOrElse("nop")
    printf("next password for %s is %s \n", pwd, n)
  }

  def part2(): Unit = {
    val pwd = "cqjxxyzz"
    val n = pwdStream(pwd).find(p => isValid(p)).getOrElse("nop")
    printf("next password for %s is %s \n", pwd, n)
  }

  def main(args: Array[String]) {
    part1()
    part2()
  }

}
