package example

trait Loop
  /**
   * imperative programming
   * using mutable variable
   * have side-effect, object fully state
   * */
def power(x: Double, n: Int): Double = {
  var res: Double = 1
  var i: Int = 0
  while(i <= n) {res = res * x; i = i + 1}
  res
}

/**
 * by-name parameters
 * by-value parameters
 * */
/**
 * command change condition
 * imperative programming*/
/**
 * do until condition become true
 * */
def doWhile(command: => Unit)(condition: => Boolean): Unit = {
  if(!condition) {
    command
    doWhile(command)(condition)
  }
  else
    () //Unit
}

class Util(body: => Unit):
  infix def until(cond: => Boolean): Unit =
    if(!cond){
      body // change state of mutable variable cond
      until(cond)
    }
    else
      ()


object Test:
  def main(args: Array[String]): Unit = {
    /**
     * case 1
     * */
    val res = power(3.14, 5)
    println(res)

    /**
     * case 2
     * */
    var x: Int = 0
    var y: Int = 2
    doWhile {
      y = y * 2
      x = x + 1
      println(y)
    }(x == 5)
  }