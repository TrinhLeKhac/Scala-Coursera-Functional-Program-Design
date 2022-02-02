import scala.collection.immutable.LazyList

object Application:
  def main(args: Array[String]):Unit = {

    /**
      * some sample of LazyList values
      * */
    val a = LazyList.cons(1, LazyList.cons(2, LazyList.cons(3, LazyList.cons(4, LazyList.empty))))
    println(a.head)

    val b = LazyList(1, 2, 3)
    println(b.head)

    /**
      * lazy range demo
      * */
    def lazyRange(lo: Int, hi: Int): TailLazyList[Int] = {
      if (lo > hi) TailLazyList.empty
      else
        TailLazyList.cons(lo, lazyRange(lo + 1, hi))
    }

    val c = lazyRange(1, 3)
    //    println(c.head)// => errors
    //    c.take(2) foreach(println) => errors

    /**
      * test lazy val, val and def
      * */
    def expr = {
      val x = {
        print("x"); 1
      } // tinh toan tu dau va luu trong memory
      lazy val y = {
        print("y"); 2
      } //khi nao can su dung moi tinh, tinh 1 lan duy nhat roi luu trong memory

      def z = {
        print("z"); 3
      } // tinh toan lai moi lan call def

      z + y + x + z + y + x
    }

    /**
      * call function expr
      * */
    expr

    /**
      * lazy list for all natural number
      * */
    def from(n: Int): LazyList[Int] = {
      LazyList.cons(n, from(n + 1))
      //      n #:: from(n + 1)
    }

    /**
      * test
      * */
    val nats = from(0)
    val res = nats.map(_ * 4)
    println()
    res.take(3).foreach(println)

    /**
      * Sieves of Eratosthenes
      * */
    def sieves(s: LazyList[Int]): LazyList[Int] = {
      s.head #:: sieves(s.tail.filter(_ % s.head != 0))
    }

    val primes = sieves(from(2))
    println("Primes number from 2: ")
    primes.take(10).foreach(println)

    /**
      * sqrt by lazy list */
    def sqrtSeq(x: Double): LazyList[Double] = {
      def improve(guess: Double): Double = (guess + x / guess) / 2

      lazy val guesses: LazyList[Double] = 1 #:: guesses.map(improve)
      guesses
    }

    def isGoodEnough(guess: Double, x: Double): Boolean = (guess * guess - x).abs < 0.0001

    val sqrt = sqrtSeq(2).filter(isGoodEnough(_, 2)).head
    println(sqrt)
  }