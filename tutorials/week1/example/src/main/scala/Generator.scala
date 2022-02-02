/**
  * Define Generator for generate the random values of type T
  * */
trait Generator[+T]:
  def generate(): T
  /**
    * define map, flatMap as method of trait Generator
    * */
  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate(): S = f(Generator.this.generate())
  }
  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate(): S = f(Generator.this.generate()).generate()
  }
/**
  * define flatMap, map, withFilter in part extension
  * */
extension[T, S](g: Generator[T])
  def emap(f: T => S): Generator[S] = new Generator[S] {
    override def generate(): S = f(g.generate())
  }
  def eflatMap(f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate(): S = f(g.generate()).generate()
  }

/**
  * application
  * */

val integers = new Generator[Int] {
  val rand = java.util.Random()
  def generate() = rand.nextInt()
}
/**
  * base booleans define
val booleans = new Generator[Boolean] {
  def generate() = integers.generate() > 0
}
  */

/**
  * after define map, flatMap
val randBool = for(i <- integers) yield (i > 0)
  * and below */
val booleans = integers.map(x => x > 0) // iterator

/** base define pairs
val pairs = new Generator[(Int, Int)] {
  def generate() = (integers.generate(), integers.generate())
}
  * */

/**
  * after define map, flatMap
  * version 1
def pairs[U, V](u: Generator[U], v: Generator[V]) = for(x <- u; y <- v) yield (x, y)

  * version 2
  def pairs[U, V](u: Generator[U], v: Generator[V]) = u.flatMap(
  x => new Generator[(U, V)] {def generate() = (x, u.generate())}
)
  * */
def pairs[U, V](u: Generator[U], v: Generator[V]) = u.flatMap(
  x => v.map(y => (x, y)))

/**
  * without define map, flatMap => define andBool and pairs method abobe => throw errors
  * */

def single[T](x: T): Generator[T] = new Generator[T] {
  override def generate(): T = x
}
def range(lo: Int, hi: Int): Generator[Int] = for(x <- integers) yield lo + x.abs % (hi - lo)
def oneOf[T](xs: T*): Generator[T] = for(idx <- range(0, xs.length)) yield xs(idx)

/**
  * implement a generator that creates random list
  * */
def lists: Generator[List[Int]] =
  for
    isEmpty <- booleans
    list <- if(isEmpty) emptyLists else nonEmptyLists
  yield list

def emptyLists = single(Nil)
def nonEmptyLists =
  for
    head <- integers
    tail <- lists
  yield head :: tail

/**
  * implement a generator that creates a random Tree
  * */

enum Tree:
  case Inner(left: Tree, right: Tree)
  case Leaf(x: Int)

def trees: Generator[Tree] =
  for
    isLeaf <- booleans
    tree <- if(isLeaf) leafs else inners
  yield tree

def leafs: Generator[Tree] = for(x <- integers) yield Tree.Leaf(x)
def inners: Generator[Tree] = for(x <- trees; y <- trees) yield Tree.Inner(x, y)

/**
  * implement a random test
  * */
def test[T](g: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit = {
  for(i <- 0 until numTimes) do {
    val value = g.generate()
    assert(test(value), s"test failed for $value")
  }
  println(s"passed $numTimes tests")
}

/**
  * instead of writing test
  * write properties
  * that idea implement in ScalaCheck tool
  * we can using function in ScalaCheck tool in simpler way
  * */
//import ScalaCheck._
//forAll {(l1: List[Int], l2: List[Int]) =>
//  l1.size + l2.size == (l1 ++ l2).size
//}

/**
  * main method to test function
  * */
object Run extends App:
//  println("Random generate integer value")
//  val randInt = integers.generate()
//  println(randInt)
//  println(s"******************************")
//
//  println("Random generate boolean value")
//  println(booleans.generate())
//  println(s"******************************")

//  println("Random generate pair value of integers")
//  val rand1, rand2 = pairs.generate()
//  println((rand1, rand2)) // generate call nextInt, iterator, rand1, rand2 => call 2 times
//  println(s"******************************")
//
//  println("Random generate pair value of integers")
//  val randPair = pairs.generate()
//  println(randPair)
//  println(s"******************************")

  val ps = pairs(integers, integers)
  println(ps.generate())

  val oneof = oneOf("orange", "watermelon", "lemon")
  println(oneof.generate())

  val testList = lists
  println(testList.generate())

  val pairslist = pairs(lists, lists)
  println(pairslist.generate())

  test(pairs(lists, lists)) {
    (xs, ys) => (xs ++ ys).length >= xs.length
  }

//  val treesTest = trees
//  print(treesTest.generate())

