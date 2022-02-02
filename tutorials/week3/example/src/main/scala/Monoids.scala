/**
  * trait SemiGroup
  * */
trait SemiGroup[T]:
  extension(x: T)
    def combine(y: T): T

/**
  * trait Monoid
  * */
trait Monoid[T] extends SemiGroup[T]:
  def unit: T
object Monoid:
  def apply[T](using m: Monoid[T]): Monoid[T] = m

  given sumMonoid: Monoid[Int] with {
    extension(x: Int)
      def combine(y: Int): Int = x + y
    def unit: Int = 0
  }

  given productMonoid: Monoid[Int] with {
    extension(x: Int)
      def combine(y: Int): Int = x * y
    def unit: Int = 1
  }

/**
  * reduce
  * */

//def reduce[T](xs: List[T])(using m: Monoid[T]): T = xs.foldLeft(m.unit)(_.combine(_))
//def reduce[T: Monoid](xs: List[T]): T = xs.foldLeft(summon[Monoid[T]].unit)(_.combine(_)) // summon[Monoid[T]] thay cho m
/**
  * with define of object Monoid*/
def reduce[T: Monoid](xs: List[T]): T = xs.foldLeft(Monoid[T].unit)(_.combine(_))
def sum(xs: List[Int]): Int = reduce(xs)(using Monoid.sumMonoid)
def product(xs: List[Int]): Int = reduce(xs)(using Monoid.productMonoid)

object Monoids:
  def main(args: Array[String]): Unit = {
    val xs = List(1, 2, 3, 4)
    val sumRes = sum(xs)
    val productRes = product(xs)
    println(sumRes)
    println(productRes)
  }